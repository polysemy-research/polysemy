{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}

module Polysemy.Internal.Lift where


------------------------------------------------------------------------------
-- | Lift a regular 'Monad' @m@ into an effect. Monadic actions in @m@ can be
-- lifted into 'Polysemy.Semantic' via 'Polysemy.runM'.
newtype Lift m z a = Lift
  { unLift :: m a
  }

