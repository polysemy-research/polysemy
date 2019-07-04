module Polysemy.Internal.Kind where

import Data.Kind

------------------------------------------------------------------------------
-- | The kind of effects.
--
-- @since 0.5.0.0
type Effect    = (Type -> Type) -> Type -> Type

------------------------------------------------------------------------------
-- | The kind of effect rows.
--
-- @since 0.5.0.0
type EffectRow = [Effect]

