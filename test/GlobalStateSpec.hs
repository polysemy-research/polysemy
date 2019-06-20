module GlobalStateSpec where

import Test.Hspec
import Polysemy
import Polysemy.State
import Polysemy.GlobalState
import Polysemy.Resource


forgetfulState
    :: Members '[State Int, Resource] r
    => Sem r (Int, Int)
forgetfulState = do
  a <- bracket
         (pure ())
         (const (put @Int 3))
         (const (put @Int 2 >> get))
  (,) <$> pure a <*> get


spec :: Spec
spec = do
  it "should forget state without GlobalState" $ do
    let res = run
            . runResource
            . evalState @Int 0
            $ forgetfulState
    res `shouldBe` (2, 2)

  it "should propagate state written in the cleanup action" $ do
    let res = run
            . runGlobalState
            . runResource
            $ runStateAsGlobal @Int 0
            $ forgetfulState
    res `shouldBe` (2, 3)

  it "should propagate state written in the cleanup action IN IO" $ do
    res <- (runM . runGlobalStateInIO)
                 .@ runResourceInIO
                 $ runStateAsGlobal @Int 0
                 $ forgetfulState
    res `shouldBe` (2, 3)

