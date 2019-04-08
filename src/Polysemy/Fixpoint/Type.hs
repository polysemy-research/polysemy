module Polysemy.Fixpoint.Type where

data Fixpoint m a where
  Fixpoint :: (a -> m a) -> Fixpoint m a

