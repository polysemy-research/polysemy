module ViewSpec where

import Polysemy
import Polysemy.State
import Polysemy.Trace
import Polysemy.View
import Test.Hspec


check_see :: Members '[View String, Trace] r => Sem r ()
check_see = trace . ("saw " ++) =<< see

spec :: Spec
spec = parallel $ do
  describe "View effect" $ do
    it "should cache views" $ do
      let a = run
            . runTraceList
            . runState @Int 0
            . viewToState @String @Int (\i -> do
                  trace $ "caching "  ++ show i
                  pure $ show i  ) $ do
              check_see
              check_see
              put @Int 3
              trace "it's lazy"
              put @Int 5
              check_see
              check_see
              get @Int

      a `shouldBe` ([ "caching 0"
                    , "saw 0"
                    , "saw 0"
                    , "it's lazy"
                    , "caching 5"
                    , "saw 5"
                    , "saw 5"
                    ], (5, 5))

