{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Embed
  ( -- * Effect
    Embed (..)

    -- * Actions
  , embed

    -- * Interpretations
  , runEmbed

    -- * Embeding monadic actions into effects
  , sendM
  ) where

import Polysemy
import Polysemy.Embed.Type (Embed (..))

makeSem ''Embed

------------------------------------------------------------------------------
-- | Given a natural transform from @m1@ to @m2@
-- run a @Embed m1@ effect by transforming it into a @Embed m2@ effect.
--
-- TODO(sandy): @since
runEmbed
    :: forall m1 m2 r a
     . Member (Embed m2) r
    => (forall x. m1 x -> m2 x)
    -> Sem (Embed m1 ': r) a
    -> Sem r a
runEmbed f = interpret $ sendM . f . unEmbed
{-# INLINE runEmbed #-}
