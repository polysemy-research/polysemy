{-# LANGUAGE NumDecimals #-}

module AsyncSpec where

import Control.Concurrent.MVar
import Control.Monad
import Polysemy
import Polysemy.Async
import Polysemy.State
import Polysemy.Trace
import Polysemy.Output
import Test.Hspec
import Data.IORef


spec :: Spec
spec = describe "async" $ do
  it "should thread state and not lock" $ do
    s_ref <- newIORef "hello"
    ts_ref <- newIORef []
    r <- runM
            . runOutputSem @String (\x -> embed $ modifyIORef ts_ref (x :))
            . traceToOutput
            . runStateIORef s_ref
            . asyncToIOFinal $ do
      let message :: Member Trace r => Int -> String -> Sem r ()
          message n msg = trace $ mconcat
            [ show n, "> ", msg ]
      ~[lock1, lock2] <- embed $
        replicateM 2 newEmptyMVar
      a1 <- async $ do
          v <- get @String
          message 1 v
          put $ reverse v

          embed $ putMVar lock1 ()
          embed $ takeMVar lock2
          get >>= message 1

          get @String

      void $ async $ do
          embed $ takeMVar lock1
          get >>= message 2
          put "pong"
          embed $ putMVar lock2 ()

      await a1 <* put "final"
    s <- readIORef s_ref
    ts <- fmap reverse $ readIORef ts_ref

    ts `shouldContain` ["1> hello", "2> olleh", "1> pong"]
    s `shouldBe` "final"
    r `shouldBe` Just "pong"
