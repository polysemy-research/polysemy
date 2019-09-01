{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module MultipleVarsSpec where

import Polysemy

import Test.Hspec

data TaggedState k s m a where
  TaggedGet :: forall k s m. TaggedState k s m s
  TaggedPut :: forall k s m. s -> TaggedState k s m ()

makeSem ''TaggedState

test :: Members '[
          TaggedState Char Int
        , TaggedState Bool Int
        ] r
      => Sem r ()
test = taggedPut @Bool 10


spec :: Spec
spec = describe "Using multiple, but ununifiable instances\
               \ of the same effect" $ do
  it "should get disambiguated and compile!" $ do
    True `shouldBe` True
