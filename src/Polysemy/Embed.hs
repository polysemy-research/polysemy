{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Embed
  ( -- * Effect
    Embed (..)

    -- * Actions
  , embed

    -- * Interpretations
  , runEmbedded
  ) where

import Polysemy
import Polysemy.Embed.Type (Embed (..))

------------------------------------------------------------------------------
-- | Given a natural transform from @m1@ to @m2@
-- run a @Embed m1@ effect by transforming it into a @Embed m2@ effect.
--
-- @since 1.0.0.0
runEmbedded
    :: forall m1 m2 r a
     . Member (Embed m2) r
    => (forall x. m1 x -> m2 x)
    -> Sem (Embed m1 ': r) a
    -> Sem r a
runEmbedded f = interpret $ embed . f . unEmbed
{-# INLINE runEmbedded #-}
