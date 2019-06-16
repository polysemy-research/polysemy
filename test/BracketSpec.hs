module BracketSpec where

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.State
import Polysemy.Trace
import Test.Hspec


runTest
  :: Sem '[Error (), Resource, State [Char], Trace, Output String] a
  -> ([String], ([Char], Either () a))
runTest = run
        . runFoldMapOutput @String (:[])
        . runTraceAsOutput
        . runState ""
        . runResource
        . runError @()


spec :: Spec
spec = parallel $ do
  describe "pure bracket" $ do
    it "persist state and call the finalizer" $ do
      let (ts, (s, e)) = runTest $ do
            bracket
              (put "allocated" >> pure ())
              (\() -> do
                get >>= trace
                put "finalized"
              )
              (\() -> do
                get >>= trace
                put "starting block"
                _ <- throw ()
                put "don't get here"
              )
      ts `shouldContain` ["allocated"]
      ts `shouldContain` ["starting block"]
      s `shouldBe` "finalized"
      e `shouldBe` Left ()

  describe "pure bracketOnError" $ do
    it "persist state and call the finalizer if there was an error" $ do
      let (ts, (s, e)) = runTest $ do
            bracketOnError
              (put "allocated" >> pure ())
              (\() -> do
                get >>= trace
                put "finalized"
              )
              (\() -> do
                get >>= trace
                put "starting block"
                _ <- throw ()
                put "don't get here"
              )
      ts `shouldContain` ["allocated"]
      ts `shouldContain` ["starting block"]
      s `shouldBe` "finalized"
      e `shouldBe` Left ()

    it "should not call the finalizer if there no error" $ do
      let (ts, (s, e)) = runTest $ do
            bracketOnError
              (put "allocated" >> pure ())
              (\() -> do
                get >>= trace
                put "finalized"
              )
              (\() -> do
                get >>= trace
                put "starting block"
                put "don't get here"
              )
      ts `shouldContain` ["allocated"]
      ts `shouldNotContain` ["starting block"]
      s `shouldBe` "don't get here"
      e `shouldBe` Right ()

