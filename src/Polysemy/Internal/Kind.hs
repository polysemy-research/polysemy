{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Kind where

import Data.Kind (Type)

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


type family Append l r where
  Append (a ': l) r = a ': (Append l r)
  Append '[] r = r
