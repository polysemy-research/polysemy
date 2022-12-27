{-# language TemplateHaskell, DerivingStrategies, GeneralizedNewtypeDeriving #-}

module ScopedSpec where

import Control.Concurrent.STM
import Polysemy
import Polysemy.Internal.Tactics
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

data HO :: Effect where
  Inc :: m a -> HO m a
  Ret :: HO m Int

makeSem ''HO

scopeHO :: () -> (() -> Sem r a) -> Sem r a
scopeHO () use =
  use ()

handleHO :: Int -> () -> HO m a -> Tactical HO m r a
handleHO n () = \case
  Inc ma -> raise . interpretH (handleHO (n + 1) ()) =<< runT ma
  Ret -> pureT n

data Esc :: Effect where
  Esc :: Esc m Int
makeSem ''Esc

data Indirect :: Effect where
  Indirect :: Indirect m Int
makeSem ''Indirect

interpretIndirect :: Member Esc r => InterpreterFor Indirect r
interpretIndirect = interpret \ Indirect -> esc

handleEsc :: Int -> Esc m a -> Sem r a
handleEsc i = \ Esc -> pure i

test_escape :: Sem (Scoped Int Esc ': r) Int
test_escape =
    scoped @Int @Esc 2
  $ interpretIndirect
  $ scoped @Int @Esc 1 indirect

spec :: Spec
spec = parallel do
  describe "Scoped" do
    it "local effects" do
      (i1, i2) <- runM $ interpretScopedWithH @'[F] @(TVar Int) @Par @E scope handleE do
        scoped @Par @E 20 do
          i1 <- e1
          i2 <- scoped @Par @E 23 e1
          pure (i1, i2)
      i1 `shouldBe` 35
      i2 `shouldBe` 38
    it "switch interpreter" do
      r <- runM $ interpretScopedH scopeHO (handleHO 1) do
        scoped_ @HO do
          inc do
            ret
      r `shouldBe` 2
    it "scoped depth" do
      r <- runM $ interpretScoped (flip ($)) handleEsc $ test_escape
      r `shouldBe` 2
      r' <- runM $ interpretScopedH'
                    (\r h -> h r)
                    (\i e -> liftT (handleEsc i e))
                 $ test_escape
      r' `shouldBe` 2
