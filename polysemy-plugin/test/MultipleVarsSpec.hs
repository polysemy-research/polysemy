{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module MultipleVarsSpec where

import Polysemy
import Polysemy.State

import Test.Hspec

data TaggedState k s m a where
  TaggedGet :: forall k s m. TaggedState k s m s
  TaggedPut :: forall k s m. s -> TaggedState k s m ()

makeSem ''TaggedState

runTaggedState :: forall k s r a
                . s
               -> Sem (TaggedState k s ': r) a
               -> Sem r (s, a)
runTaggedState s =
    (runState s .)
  $ reinterpret
  $ \case
    TaggedGet -> get
    TaggedPut s -> put s

test :: Members '[
          TaggedState Char Int
        , TaggedState Bool Int
        ] r
     => Sem r ()
test = do
  taggedPut @Bool 10
  taggedPut @Char (-10)

spec :: Spec
spec = describe "Using multiple, but ununifiable instances\
               \ of the same effect" $ do
  it "should get disambiguated and compile, \
     \and actions should target the right effects." $ do
    let
      res1 =
          run
        . runTaggedState @Char 0
        . runTaggedState @Bool 7
        $ test
      res2 =
          run
        . runTaggedState @Bool 0
        . runTaggedState @Char 7
        $ test
      res3 =
          run
        . runTaggedState @Bool 0
        . runTaggedState @Char 7
        $ do
          taggedPut @Bool 10
          taggedPut @Char (-10)
    res1 `shouldBe` (-10, (10, ()))
    res2 `shouldBe` (10, (-10, ()))
    res3 `shouldBe` (10, (-10, ()))
