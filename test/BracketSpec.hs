module BracketSpec where

import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.State
import Polysemy.Trace
import Test.Hspec


runTest
  :: Sem '[Error (), Resource, State [Char], Trace] a
  -> ([String], ([Char], Either () a))
runTest = run
        . runTraceList
        . runState ""
        . runResource
        . runError @()

runTest2
  :: Sem '[Error (), Resource, State [Char], Trace, Embed IO] a
  -> IO ([String], ([Char], Either () a))
runTest2 = runM
         . runTraceList
         . runState ""
         . resourceToIO
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


  describe "io dispatched bracket" $ do
    it "persist state and call the finalizer" $ do
      (ts, (s, e)) <- runTest2 $ do
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

    it "should not lock when done recursively" $ do
      (ts, (s, e)) <- runTest2 $ do
            bracket
              (put "hello 1")
              (\() -> do
                get >>= trace
                put "finished"
              )
              (\() -> do
                get >>= trace
                bracket (put "hello 2")
                        (const $ do
                          get >>= trace
                          put "goodbye 2"
                        )
                        (const $ do
                          get >>= trace
                          put "RUNNING"
                          throw ()
                        )
                -- This doesn't run due to the thrown error above
                get >>= trace
                put "goodbye 1"
              )
      ts `shouldContain` [ "hello 1"
                         , "hello 2"
                         , "RUNNING"
                         , "goodbye 2"
                         ]
      s `shouldBe` "finished"
      e `shouldBe` Left ()

