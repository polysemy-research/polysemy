module Polysemy.Internal.Fixpoint where

------------------------------------------------------------------------------
-- | An effect for providing 'Control.Monad.Fix.mfix'.
data Fixpoint m a where
  Fixpoint :: (a -> m a) -> Fixpoint m a

