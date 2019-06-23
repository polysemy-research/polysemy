{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Dispatch where

import           Control.Concurrent (threadDelay)
import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import qualified Control.Exception as X
import           Control.Monad
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union
import           Polysemy.Resource
import           Polysemy.State
import Data.Function


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
  Async :: m a -> Async m (A.Async (Maybe a))
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
    pureT $ fmap (inspect ins) res


runResource'
    :: forall r a
     . ( LastMembers (Lift IO) r
       , Member (Lift IO) r
       )
    => Sem (Resource ': r) a
    -> Sem r a
runResource' = interpretH $ \case
  Bracket a b c -> do
    ma <- runT a
    mb <- bindT b
    mc <- bindT c

    withLowerToIO $ \lower finish -> do
      let done :: Sem (Resource ': r) x -> IO x
          done = lower . runResource'
      X.bracket
          (done ma)
          (\x -> done (mb x) >> finish)
          (done . mc)

  BracketOnError a b c -> do
    ma <- runT a
    mb <- bindT b
    mc <- bindT c

    withLowerToIO $ \lower finish -> do
      let done :: Sem (Resource ': r) x -> IO x
          done = lower . runResource'
      X.bracketOnError
          (done ma)
          (\x -> done (mb x) >> finish)
          (done . mc)


withLowerToIO
    :: ( LastMembers (Lift IO) r
       , Member (Lift IO) r
       )
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


test :: IO ()
test = do
  (inchan, outchan) <- newChan
  void $ A.async $ do
    runM $ runState "hello" $ receiveEverything outchan

  let message :: Member (Lift IO) r => Int -> String -> Sem r ()
      message n msg = sendM $ putStrLn $ mconcat
        [ "thread ", show n, "> ", msg ]

  res <- runM $ dispatchEverything inchan $ runAsync inchan $ do
    a1 <- async $ do
      v <- get @String
      message 1 v
      put $ reverse v

      sendM $ threadDelay 1e6
      v' <- get @String
      message 1 v'

      sendM $ threadDelay 1e6
      get @String

    void $ async $ do
      sendM $ threadDelay 5e5
      v <- get @String
      message 2 v
      put "pong"

    await a1

  print res




