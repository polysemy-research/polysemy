module BracketSpec where

import Control.Monad
import Polysemy
import Polysemy.Error
import Polysemy.Output
import Polysemy.Resource
import Polysemy.State
import Polysemy.Trace
import Test.Hspec
import Unsafe.Coerce



spec :: Spec
spec = parallel $ do
  testAllThree "persist state and call the finalizer"
      (\(ts, (s, e)) -> do
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
        ts `shouldBe` ["allocated", "starting block"]
      ) $ do
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

  testAllThree "persist state and call the finalizer with bracketOnError"
      (\(ts, (s, e)) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldContain` ["starting block"]
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
      ) $ do
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

  testAllThree "should not call the finalizer if there no error"
      (\(ts, (s, e)) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldNotContain` ["starting block"]
        s `shouldBe` "don't get here"
        e `shouldBe` Right ()
      ) $ do
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

  testAllThree "should call the finalizer on Error"
      (\(ts, (s, e)) -> do
        ts `shouldContain` ["beginning transaction"]
        ts `shouldContain` ["rolling back transaction"]
        s `shouldBe` ""
        e `shouldBe` Left ()
      ) $ do
    withTransaction $ do
      void $ throw ()
      pure "hello"

  testTheIOTwo "io dispatched bracket"
      (\(ts, (s, e)) -> do
        ts `shouldContain` ["allocated"]
        ts `shouldContain` ["starting block"]
        s `shouldBe` "finalized"
        e `shouldBe` Left ()
      ) $ do
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

  testTheIOTwo "should not lock when done recursively"
      (\(ts, (s, e)) -> do
        ts `shouldContain` [ "hello 1"
                           , "hello 2"
                           , "RUNNING"
                           , "goodbye 2"
                           ]
        s `shouldBe` "finished"
        e `shouldBe` Left ()
      ) $ do
    bracket
      (put "hello 1")
      (\() -> do
        get >>= trace
        put "finished"
      )
      (\() -> do
        get >>= trace
        void $
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


------------------------------------------------------------------------------


runTest
  :: Sem '[Error (), Resource, State [Char], Trace] a
  -> IO ([String], ([Char], Either () a))
runTest = pure
        . run
        . runTraceList
        . runState ""
        . runResource
        . runError @()

runTest2
  :: Sem '[Error (), Resource, State [Char], Trace, Output String, Embed IO] a
  -> IO ([String], ([Char], Either () a))
runTest2 = runM
         . ignoreOutput
         . runTraceList
         . runState ""
         . resourceToIO
         . runError @()

runTest3
  :: Sem '[Error (), Resource, State [Char], Trace, Output String, Embed IO, Final IO] a
  -> IO ([String], ([Char], Either () a))
runTest3 = runFinal
         . embedToFinal
         . outputToIOMonoid (:[])
         . traceToOutput
         . stateToIO ""
         . resourceToIOFinal
         . runError @()


testAllThree
    :: String
    -> (([String], ([Char], Either () a)) -> Expectation)
    -> (Sem '[Error (), Resource, State [Char], Trace] a)
    -> Spec
testAllThree name k m = do
  describe name $ do
    it "via runResource" $ do
      z <- runTest m
      k z
    -- NOTE(sandy): These unsafeCoerces are safe, because we're just weakening
    -- the end of the union
    it "via resourceToIO" $ do
      z <- runTest2 $ unsafeCoerce m
      k z
    it "via resourceToIOFinal" $ do
      z <- runTest3 $ unsafeCoerce m
      k z


testTheIOTwo
    :: String
    -> (([String], ([Char], Either () a)) -> Expectation)
    -> (Sem '[Error (), Resource, State [Char], Trace, Output String, Embed IO] a)
    -> Spec
testTheIOTwo name k m = do
  describe name $ do
    it "via resourceToIO" $ do
      z <- runTest2 m
      k z
    -- NOTE(sandy): This unsafeCoerces are safe, because we're just weakening
    -- the end of the union
    it "via resourceToIOFinal" $ do
      z <- runTest3 $ unsafeCoerce m
      k z


withTransaction :: (Member Resource r, Member Trace r) => Sem r a -> Sem r a
withTransaction m =
  bracketOnError
    (trace "beginning transaction")
    (const $ trace "rolling back transaction")
    (const $ m <* trace "committing transaction")
