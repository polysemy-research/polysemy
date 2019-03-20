{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE KindSignatures #-}

module Polysemy.Lift where

import Polysemy.Effect


------------------------------------------------------------------------------
-- | Lift a regular 'Monad' @m@ into an effect. Monadic actions in @m@ can be
-- lifted into 'Polysemy.Semantic' via 'Polysemy.runM'.
newtype Lift m (z :: * -> *) a = Lift
  { unLift :: m a
  }
  deriving (Functor, Applicative, Monad) via m
  deriving Effect

