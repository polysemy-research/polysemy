{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module AmbiguousSpec where

import Polysemy
import Test.Hspec
import Polysemy.State

ambiguous :: Members '[
              State Int
            , State String
            ] r
          => Sem r ()
ambiguous = put 10

spec :: Spec
spec = describe "example" $ do
  it "should compile!" $ do
    let z = run . runState 0 . runState "hello" $ ambiguous
    z `shouldBe` (10, ("hello", ()))

