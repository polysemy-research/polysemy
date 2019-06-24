module Polysemy.Internal.Kind where

import Data.Kind

------------------------------------------------------------------------------
-- | The kind of effects.
--
-- TODO(sandy): @since
type Effect    = (Type -> Type) -> Type -> Type

------------------------------------------------------------------------------
-- | The kind of effect rows.
--
-- TODO(sandy): @since
type EffectRow = [Effect]

