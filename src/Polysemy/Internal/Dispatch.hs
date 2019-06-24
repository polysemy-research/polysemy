{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Internal.Dispatch where

import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad
import           Data.Function
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union


data Request r = forall a. Request
  { responseMVar :: MVar (Sem '[Lift IO] a)
  , request      :: Union r (Sem r) a
  }


dispatchEverything
    :: LastMember (Lift IO) r
    => InChan (Request r)
    -> Sem r a
    -> Sem '[Lift IO] a
dispatchEverything chan (Sem m) = Sem $ \k -> m $ \u -> do
  case decompLast u of
    Left x -> usingSem k $ join $ sendM $ do
      mvar <- newEmptyMVar
      writeChan chan $ Request mvar x
      takeMVar mvar
    Right y -> k $ hoist (dispatchEverything chan) y


receiveEverything
    :: LastMember (Lift IO) r
    => OutChan (Request r)
    -> Sem r a
receiveEverything chan = Sem $ \k -> forever $ do
  Request mvar req <- k $ inj $ Lift $ readChan chan
  resp <- k req
  k $ inj $ Lift $ putMVar mvar $ pure resp


withLowerToIO
    :: LastMember (Lift IO) r
    => ((forall x. Sem r x -> IO x) -> IO () -> IO a)
    -> Sem (WithTactics e f m r) a
withLowerToIO action = do
  (inchan, outchan) <- sendM newChan
  signal <- sendM newEmptyMVar

  res <- sendM $ A.async $ do
    a <- action (runM . dispatchEverything inchan)
                (putMVar signal ())
    putMVar signal ()
    pure a

  fix $ \me -> do
    raced <- sendM $ A.race (takeMVar signal) $ readChan outchan
    case raced of
      Left () -> sendM $ A.wait res
      Right (Request mvar req) -> do
        resp <- liftSem $ weaken $ hoist raise req
        sendM $ putMVar mvar $ pure resp
        me


data Async m a where
  Async :: m a -> Async m (A.Async (Maybe a))
  Await :: A.Async a -> Async m a

makeSem ''Async


runAsync
    :: LastMember (Lift IO) r
    => InChan (Request r)
    -> Sem (Async ': r) a
    -> Sem r a
runAsync chan = interpretH $ \case
  Await a -> (sendM $ A.wait a) >>= pureT
  Async ma -> do
    ma' <- runT ma
    res <- sendM $ A.async
                 $ runM
                 $ dispatchEverything chan
                 $ runAsync chan ma'
    ins <- getInspectorT
    pureT $ fmap (inspect ins) res
