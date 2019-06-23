{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Dispatch where

import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Data.Maybe
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union
import           Polysemy.State


data Request r end = forall a. Request
  { responseMVar :: MVar (Sem '[end] a)
  , request      :: Union r (Sem r) a
  }

dispatchEverything
    :: ( LastMembers end r
       , Member (Lift IO) '[end]
       )
    => InChan (Request r end)
    -> Sem r a
    -> Sem '[end] a
dispatchEverything chan (Sem m) = Sem $ \k -> m $ \u -> do
  case decompLast u of
    Left x -> usingSem k $ join $ sendM $ do
      mvar <- newEmptyMVar
      writeChan chan $ Request mvar x
      takeMVar mvar
    Right y -> k $ hoist (dispatchEverything chan) y


receiveEverything
    :: ( LastMembers end r
       , Member (Lift IO) r
       )
    => OutChan (Request r end)
    -> Sem r a
receiveEverything chan = Sem $ \k -> forever $ do
  Request mvar req <- k $ inj $ Lift $ readChan chan
  resp <- k req
  k $ inj $ Lift $ putMVar mvar $ pure resp


data Async m a where
  Async :: m a -> Async m (A.Async a)
  Await :: A.Async a -> Async m a

makeSem ''Async


runAsync
    :: ( LastMembers (Lift IO) r
       , Member (Lift IO) r
       )
    => InChan (Request r (Lift IO))
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
    -- safe as long as runAsync is run before runError
    let res' = fmap (fromJust . inspect ins) res
    pureT res'


test :: IO ()
test = do
  (inchan, outchan) <- newChan
  void $ A.async $ do
    runM $ runState "hello" $ receiveEverything outchan

  res <- runM $ dispatchEverything inchan $ runAsync inchan $ do
    a1 <- async $ do
      v <- get @String
      sendM $ putStrLn $ "thread 1> " ++ v
      put $ reverse v
      sendM $ threadDelay 1e6
      v' <- get @String
      sendM $ putStrLn $ "thread 1> " ++ v'
      sendM $ threadDelay 1e6
      get @String

    void $ async $ do
      sendM $ threadDelay 5e5
      v <- get @String
      sendM $ putStrLn $ "thread 2> " ++ v
      put "pong"
    await a1

  print res




