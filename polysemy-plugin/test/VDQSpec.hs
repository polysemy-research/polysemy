{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module VDQSpec where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Resource
import Test.Hspec

data Select a = Select a

data DBAction whichDb m a where
  DoSelect :: Select a -> DBAction whichDb m (Maybe a)

makeSem ''DBAction

runDBAction :: Sem (DBAction which ': r) a -> Sem r a
runDBAction = interpret $ \case
  DoSelect (Select a) -> pure $ Just a

spec :: Spec
spec = describe "example" $ do
  it "should compile!" $ do
    let z = run . runDBAction $ doSelect $ Select True
    z `shouldBe` Just True

