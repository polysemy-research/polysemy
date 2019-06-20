module Polysemy.Internal.Kind where

import Data.Kind

------------------------------------------------------------------------------
-- | The kind of effects.
type Effect    = (Type -> Type) -> Type -> Type

------------------------------------------------------------------------------
-- | The kind of effect rows.
type EffectRow = [Effect]

