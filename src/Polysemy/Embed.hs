{-# LANGUAGE TemplateHaskell #-}

-- | Description: Interpreters for the effect 'Embed'
module Polysemy.Embed
  ( -- * Effect
    Embed (..)

    -- * Actions
  , embed

    -- * Interpretations
  , embedToEmbed
  ) where

import Polysemy
import Data.Coerce

------------------------------------------------------------------------------
-- | Given a natural transform from @m1@ to @m2@
-- run a @Embed m1@ effect by transforming it into a @Embed m2@ effect.
--
-- @since 1.0.0.0
embedToEmbed
    :: forall m1 m2 r a
     . Member (Embed m2) r
    => (forall x. m1 x -> m2 x)
    -> Sem (Embed m1 ': r) a
    -> Sem r a
embedToEmbed f = transform (coerce (f @x)
                            :: forall z x. Embed m1 z x -> Embed m2 z x)
{-# INLINE embedToEmbed #-}
