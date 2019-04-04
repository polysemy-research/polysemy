{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Writer where

import Polysemy
import Polysemy.Effect.New
import Polysemy.Output
import Polysemy.State

data Writer o m a
  = Tell o a
  | ∀ x. Listen (m x) (o -> x -> a)
  | ∀ x. Censor (o -> o) (m x) (x -> a)

deriving instance Functor (Writer o m)

instance Effect (Writer o) where
  weave s _ (Tell o k) = Tell o $ k <$ s
  weave s distrib (Listen m k) =
    Listen (distrib $ m <$ s) (fmap fmap k)
  weave s distrib (Censor f m k) =
    Censor f (distrib $ m <$ s) (fmap k)
  {-# INLINE weave #-}

  hoist = defaultHoist
  {-# INLINE hoist #-}

makeSemantic ''Writer


runOutputAsWriter :: Semantic (Output o ': r) a -> Semantic (Writer o ': r) a
runOutputAsWriter = reinterpret \case
  Output o k -> tell o >> pure k
{-# INLINE runOutputAsWriter #-}


inlineRecursiveCalls [d|
  runWriter
      :: Monoid o
      => Semantic (Writer o ': r) a
      -> Semantic r (o, a)
  runWriter = runState mempty . reinterpret \case
    Tell o k -> do
      modify (<> o)
      pure k
    Listen m k -> do
      ~(o, a) <- raise $ runWriter m
      pure $ k o a
    Censor f m k -> do
      ~(o, a) <- raise $ runWriter m
      modify (<> f o)
      pure $ k a
  |]

