{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Bundle
  ( -- * Effect
    Bundle (..)
    -- * Actions
  , sendBundle
  , injBundle
    -- * Interpretations
  , runBundle
  , subsumeBundle
    -- * Miscellaneous
  , KnownList
  ) where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Bundle
import Polysemy.Internal.Union

------------------------------------------------------------------------------
-- | An effect for collecting multiple effects into one effect.
--
-- Useful for effect newtypes -- effects defined through creating a newtype
-- over an existing effect, and then defining actions and interpretations on
-- the newtype by using 'rewrite' and 'transform'.
--
-- By making a newtype of 'Bundle', it's possible to wrap multiple effects in
-- one newtype.
data Bundle r m a where
  Bundle :: ElemOf e r -> e m a -> Bundle r m a

------------------------------------------------------------------------------
-- | Injects an effect into a 'Bundle'. Useful together with 'transform'.
injBundle :: forall e r m a. Member e r => e m a -> Bundle r m a
injBundle = Bundle membership
{-# INLINE injBundle #-}

------------------------------------------------------------------------------
-- | Send uses of an effect to a 'Bundle' containing that effect.
sendBundle
  :: forall e r' r a
   . (Member e r', Member (Bundle r') r)
  => Sem (e ': r) a
  -> Sem r a
sendBundle = hoistSem $ \u -> case decomp u of
  Right (Weaving e s wv ex ins) ->
    injWeaving $
      Weaving (Bundle (membership @e @r') e) s (sendBundle @e @r' . wv) ex ins
  Left g -> hoist (sendBundle @e @r') g
{-# INLINE sendBundle #-}

------------------------------------------------------------------------------
-- | Run a @'Bundle' r@ by prepending @r@ to the effect stack.
runBundle
  :: forall r' r a
   . KnownList r'
  => Sem (Bundle r' ': r) a
  -> Sem (Append r' r) a
runBundle = hoistSem $ \u -> hoist runBundle $ case decomp u of
  Right (Weaving (Bundle pr e) s wv ex ins) ->
    Union (extendMembership @_ @r pr) $ Weaving e s wv ex ins
  Left g -> weakenList @r' @r g
{-# INLINE runBundle #-}

------------------------------------------------------------------------------
-- | Run a @'Bundle' r@ if the effect stack contains all effects of @r@.
subsumeBundle
  :: forall r' r a
   . Members r' r
  => Sem (Bundle r' ': r) a
  -> Sem r a
subsumeBundle = hoistSem $ \u -> hoist subsumeBundle $ case decomp u of
  Right (Weaving (Bundle pr e) s wv ex ins) ->
    Union (subsumeMembership pr) (Weaving e s wv ex ins)
  Left g -> g
{-# INLINE subsumeBundle #-}
