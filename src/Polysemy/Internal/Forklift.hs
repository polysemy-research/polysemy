{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Internal.Forklift where

import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Function
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union


data Forklift r = forall a. Forklift
  { responseMVar :: MVar (Sem '[Lift IO] a)
  , request      :: Union r (Sem r) a
  }


viaForklift
    :: LastMember (Lift IO) r
    => InChan (Forklift r)
    -> Sem r a
    -> Sem '[Lift IO] a
viaForklift chan (Sem m) = Sem $ \k -> m $ \u -> do
  case decompLast u of
    Left x -> usingSem k $ join $ sendM $ do
      mvar <- newEmptyMVar
      writeChan chan $ Forklift mvar x
      takeMVar mvar
    Right y -> k $ hoist (viaForklift chan) y


withLowerToIO
    :: LastMember (Lift IO) r
    => ((forall x. Sem r x -> IO x) -> IO () -> IO a)
    -> Sem r a
withLowerToIO action = do
  (inchan, outchan) <- sendM newChan
  signal <- sendM newEmptyMVar

  res <- sendM $ A.async $ do
    a <- action (runM . viaForklift inchan)
                (putMVar signal ())
    putMVar signal ()
    pure a

  fix $ \me -> do
    raced <- sendM $ A.race (takeMVar signal) $ readChan outchan
    case raced of
      Left () -> sendM $ A.wait res
      Right (Forklift mvar req) -> do
        resp <- liftSem req
        sendM $ putMVar mvar $ pure resp
        me

