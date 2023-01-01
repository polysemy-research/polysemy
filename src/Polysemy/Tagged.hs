{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Description: The 'Tagged' effect and its interpreters
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

import Data.Coerce
import Polysemy
import Polysemy.Internal.Utils

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
--   'tag' \@k \@('Polysemy.Reader.Reader' i) $ 'Polysemy.Reader.local' @i f ('raise' m)
-- @
--
tag
    :: forall k e r a
     . Member (Tagged k e) r
    => Sem (e ': r) a
    -> Sem r a
tag = transform @e @(Tagged k e) coerce


------------------------------------------------------------------------------
-- | A reinterpreting version of 'tag'.
tagged
    :: forall k e r a
     . Sem (e ': r) a
    -> Sem (Tagged k e ': r) a
tagged = coerceEffs
{-# INLINE tagged #-}



------------------------------------------------------------------------------
-- | Run a @'Tagged' k e@ effect through reinterpreting it to @e@
untag
    :: forall k e r a
     . Sem (Tagged k e ': r) a
    -> Sem (e ': r) a
untag = coerceEffs
{-# INLINE untag #-}


------------------------------------------------------------------------------
-- | Transform a @'Tagged' k1 e@ effect into a @'Tagged' k2 e@ effect
retag
    :: forall k1 k2 e r a
     . Member (Tagged k2 e) r
    => Sem (Tagged k1 e ': r) a
    -> Sem r a
retag = transform (\(Tagged e) -> Tagged @k2 e)
