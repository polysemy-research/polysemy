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
  , consBundle
  , unconsBundle
  , mapMembershipBundle
    -- * Miscellaneous
  , KnownList
  ) where

import Polysemy
import Polysemy.HigherOrder
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.Bundle (simpleSubsumeMembership)
import Polysemy.Internal.Sing

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
  :: forall e l r a
   . (Member e l, Member (Bundle l) r)
  => Sem (e ': r) a
  -> Sem r a
sendBundle = sendBundleUsing (membership @e @l)
{-# INLINE sendBundle #-}

------------------------------------------------------------------------------
-- | Send uses of an effect to a 'Bundle' containing that effect, given an
-- explicit proof that that the bundle contains that effect.
sendBundleUsing
  :: forall e l r a
   . Member (Bundle l) r
  => ElemOf e l
  -> Sem (e ': r) a
  -> Sem r a
sendBundleUsing pr = transform (Bundle pr)
{-# INLINE sendBundleUsing #-}

------------------------------------------------------------------------------
-- | Send uses of @e@ and @'Bundle' l@ to @'Bundle' (e ': r)@
consBundle :: forall e l r a
            . Sem (e ': Bundle l ': r) a
           -> Sem (Bundle (e ': l) ': r) a
consBundle =
    mapMembershipBundle @(e ': l) There
  . sendBundleUsing (Here @e @(e ': l))
  . raiseUnder2

------------------------------------------------------------------------------
-- | Decompose @'Bundle' (e ': l)@ into @e@ and @Bundle l@ by placing uses
-- of @e@ on top of the effect stack, and sending uses of any effect of @l@ to
-- @'Bundle' l@
unconsBundle :: forall e l r a
              . Sem (Bundle (e ': l) ': r) a
             -> Sem (e ': Bundle l ': r) a
unconsBundle = reinterpret2H \case
  Bundle Here e -> propagate e
  Bundle (There pr) e -> propagateUsing (There Here) (Bundle pr e)

------------------------------------------------------------------------------
-- | Send uses of @'Bundle' l@ to @'Bundle' r@ given an explicit membership
-- proof transformation
mapMembershipBundle :: forall l' l r a
                     . Member (Bundle l') r
                    => (forall e. ElemOf e l -> ElemOf e l')
                    -> Sem (Bundle l ': r) a
                    -> Sem r a
mapMembershipBundle t = transform (\(Bundle pr e) -> Bundle (t pr) e)
{-# INLINE mapMembershipBundle #-}

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
-- | Run a @'Bundle' l@ if the effect stack contains all effects of @r@.
subsumeBundle
  :: forall r' r a
   . Members r' r
  => Sem (Bundle r' ': r) a
  -> Sem r a
subsumeBundle = hoistSem $ \u -> hoist subsumeBundle $ case decomp u of
  Right (Weaving (Bundle pr e) mkT lwr ex) ->
    Union (simpleSubsumeMembership pr) (Weaving e mkT lwr ex)
  Left g -> g
{-# INLINE subsumeBundle #-}
