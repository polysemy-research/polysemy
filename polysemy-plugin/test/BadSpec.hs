{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

module BadSpec where

import Polysemy
import Polysemy.State
import Test.Hspec
import Test.ShouldNotTypecheck

data KVStore k v m a where
  GetKV :: k -> KVStore k v m (Maybe v)

makeSem ''KVStore

runKVStore :: [(k, v)] -> InterpreterFor (KVStore k v) r
runKVStore = interpret $ \case
  GetKV _ -> pure Nothing

positivePos :: Member (KVStore k v) r => Sem r (Maybe v)
positivePos = do
  getKV "hello"

negativePos :: Member (KVStore String v) r => Sem r (Maybe Bool)
negativePos = do
  getKV "hello"

badState :: Member (State a) r => Sem r ()
badState = put ()


spec :: Spec
spec = do
  describe "incorrectly polymorphic constraint" $ do
    it "should not typecheck in positive position" $ do
      shouldNotTypecheck (run . runKVStore [("hello", True)]
        $ positivePos @String @Bool @'[KVStore String Bool])
    it "should not typecheck in negative position" $ do
      shouldNotTypecheck (run . runKVStore [("hello", True)]
        $ negativePos @Bool @'[KVStore String Bool])
    it "should not typecheck badly polymorphic State" $ do
      shouldNotTypecheck (run . runState ()
        $ badState @() @'[State ()])

