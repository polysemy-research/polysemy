{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Lift
  ( -- * Effect
    Lift (..)

    -- * Actions
  , lift

    -- * Interpretations
  , runLift

    -- * Lifting monadic actions into effects
  , sendM
  ) where

import           Polysemy
import           Polysemy.Lift.Type (Lift(..))

makeSem ''Lift

------------------------------------------------------------------------------
-- | Given a natural transform from @m1@ to @m2@
-- run a @Lift m1@ effect by transforming it into a @Lift m2@ effect.
--
-- TODO(sandy): @since
runLift :: Member (Lift m2) r => (forall x. m1 x -> m2 x) -> Sem (Lift m1 ': r) a -> Sem r a
runLift f = interpret $ sendM . f . unLift
{-# INLINE runLift #-}
