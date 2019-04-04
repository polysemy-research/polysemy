{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

module Polysemy.Fixpoint.Type where

import Polysemy.Effect

data Fixpoint m a
  = ∀ x. Fixpoint (x -> m x) (x -> a)

deriving instance Functor (Fixpoint m)

instance Effect Fixpoint where
  weave _ distrib (Fixpoint f k) =
    Fixpoint (\sx -> distrib $ fmap f sx) (fmap k)
  hoist nat (Fixpoint f k) = Fixpoint (fmap nat f) k

