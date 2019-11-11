{-# LANGUAGE NumDecimals #-}

module InputSpec where

import Control.Concurrent
import Polysemy
import Polysemy.Input
import Test.Hspec


spec :: Spec
spec = describe "input" $ do
  it "should race" $ do
    mvar <- newEmptyMVar

    results
      <- runFinal
       . embedToFinal @IO
       . raceInput
           (runInputSem $ embed (threadDelay 1e4) >> pure "timeout")
           (runInputSem $ embed $ takeMVar mvar)
       $ do
      a <- input
      b <- input
      embed $ putMVar mvar "mvar"
      c <- input
      d <- input
      embed $ putMVar mvar "mvar"
      e <- input
      pure [a,b,c,d,e]

    results `shouldBe`
      [ "timeout"
      , "timeout"
      , "mvar"
      , "timeout"
      , "mvar"
      ]
