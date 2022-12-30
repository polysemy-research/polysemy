{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Description: The 'Bundle' effect for bundling effects
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
import Polysemy.Internal.Union
import Polysemy.Internal.Bundle (subsumeMembership)
import Polysemy.Internal.Sing (KnownList (singList))

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
  Right (Weaving e mkT lwr ex) ->
    injWeaving $
      Weaving (Bundle (membership @e @r') e)
              (\n -> mkT (n . sendBundle @e @r'))
              lwr
              ex
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
  Right (Weaving (Bundle pr e) mkT lwr ex) ->
    Union (extendMembershipRight @r' @r pr) $ Weaving e mkT lwr ex
  Left g -> weakenList @r' @r (singList @r') g
{-# INLINE runBundle #-}

------------------------------------------------------------------------------
-- | Run a @'Bundle' r@ if the effect stack contains all effects of @r@.
subsumeBundle
  :: forall r' r a
   . Members r' r
  => Sem (Bundle r' ': r) a
  -> Sem r a
subsumeBundle = hoistSem $ \u -> hoist subsumeBundle $ case decomp u of
  Right (Weaving (Bundle pr e) mkT lwr ex) ->
    Union (subsumeMembership pr) (Weaving e mkT lwr ex)
  Left g -> g
{-# INLINE subsumeBundle #-}
