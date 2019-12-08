{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Tagged
  (
    -- * Effect
    Tagged (..)

    -- * Actions
  , tag
  , tagged

    -- * Interpretations
  , untag
  , retag
  ) where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | An effect for annotating effects and disambiguating identical effects.
newtype Tagged k e m a where
  Tagged :: forall k e m a. e m a -> Tagged k e m a


------------------------------------------------------------------------------
-- | Tag uses of an effect, effectively gaining access to the
-- tagged effect locally.
--
-- This may be used to create @tagged-@ variants of regular actions.
--
-- For example:
--
-- @
-- taggedLocal :: forall k i r a
--              . 'Member' ('Tagged' k ('Polysemy.Reader.Reader' i)) r
--             => (i -> i)
--             -> 'Sem' r a
--             -> 'Sem' r a
-- taggedLocal f m =
--   'tag' @k @('Polysemy.Reader.Reader' i) $ 'Polysemy.Reader.local' @i f ('raise' m)
-- @
--
tag
    :: forall k e r a
     . Member (Tagged k e) r
    => Sem (e ': r) a
    -> Sem r a
tag = hoistSem $ \u -> case decomp u of
  Right (Weaving e s wv ex ins) ->
    injWeaving $ Weaving (Tagged @k e) s (tag @k . wv) ex ins
  Left g -> hoist (tag @k) g
{-# INLINE tag #-}


------------------------------------------------------------------------------
-- | A reinterpreting version of 'tag'.
tagged
    :: forall k e r a
     . Sem (e ': r) a
    -> Sem (Tagged k e ': r) a
tagged = hoistSem $ \u ->
  case decompCoerce u of
    Right (Weaving e s wv ex ins) ->
      injWeaving $ Weaving (Tagged @k e) s (tagged @k . wv) ex ins
    Left g -> hoist (tagged @k) g
{-# INLINE tagged #-}



------------------------------------------------------------------------------
-- | Run a @'Tagged' k e@ effect through reinterpreting it to @e@
untag
    :: forall k e r a
     . Sem (Tagged k e ': r) a
    -> Sem (e ': r) a
-- TODO(KingoftheHomeless): I think this is safe to replace with 'unsafeCoerce',
-- but doing so probably worsens performance, as it hampers optimizations.
-- Once GHC 8.10 rolls out, I will benchmark and compare.
untag = hoistSem $ \u -> case decompCoerce u of
  Right (Weaving (Tagged e) s wv ex ins) ->
    Union Here (Weaving e s (untag . wv) ex ins)
  Left g -> hoist untag g
{-# INLINE untag #-}


------------------------------------------------------------------------------
-- | Transform a @'Tagged' k1 e@ effect into a @'Tagged' k2 e@ effect
retag
    :: forall k1 k2 e r a
     . Member (Tagged k2 e) r
    => Sem (Tagged k1 e ': r) a
    -> Sem r a
retag = hoistSem $ \u -> case decomp u of
  Right (Weaving (Tagged e) s wv ex ins) ->
    injWeaving $ Weaving (Tagged @k2 e) s (retag @_ @k2 . wv) ex ins
  Left g -> hoist (retag @_ @k2) g
{-# INLINE retag #-}

