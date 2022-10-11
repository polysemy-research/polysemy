{-# language TemplateHaskell, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module ScopedSpec where

import Control.Concurrent.STM
import Polysemy
import Polysemy.Scoped
import Test.Hspec

newtype Par =
  Par { unPar :: Int }
  deriving stock (Eq, Show)
  deriving newtype (Num, Real, Enum, Integral, Ord)

data E :: Effect where
  E1 :: E m Int
  E2 :: E m Int

makeSem ''E

data F :: Effect where
  F :: F m Int

makeSem ''F

handleE ::
  Member (Embed IO) r =>
  TVar Int ->
  E m a ->
  Tactical effect m (F : r) a
handleE tv = \case
  E1 -> do
    i1 <- embed (readTVarIO tv)
    i2 <- f
    pureT (i1 + i2 + 10)
  E2 ->
    pureT (-1)

interpretF ::
  Member (Embed IO) r =>
  TVar Int ->
  InterpreterFor F r
interpretF tv =
  interpret \ F -> do
    embed (atomically (writeTVar tv 7))
    pure 5

scope ::
  Member (Embed IO) r =>
  Par ->
  (TVar Int -> Sem (F : r) a) ->
  Sem r a
scope (Par n) use = do
  tv <- embed (newTVarIO n)
  interpretF tv (use tv)

spec :: Spec
spec = parallel do
  describe "Scoped" do
    it "local effects" do
      (i1, i2) <- runM $ interpretScopedWithH @'[F] @(TVar Int) @Par @E scope handleE do
        scoped @Par @E 20 do
          i1 <- e1
          i2 <- scoped @Par @E 23 e1
          pure (i1, i2)
      35 `shouldBe` i1
      38 `shouldBe` i2
