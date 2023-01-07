{-# LANGUAGE AllowAmbiguousTypes #-}
-- | The auxiliary effect 'Opaque' used by interpreters of 'Polysemy.Scoped.Scoped'
module Polysemy.Opaque (
  -- * Effect
  Opaque(..),

  -- * Interpreters
  toOpaque,
  fromOpaque,
  toOpaqueAt,
  fromOpaqueAt,
  collectOpaqueBundleAt,
  runOpaqueBundleAt,
  ) where

import Polysemy
import Polysemy.Internal
import Polysemy.Bundle
import Polysemy.Membership
import Polysemy.Internal.Union
import Polysemy.Internal.Opaque
import Polysemy.Newtype

-- | Wrap 'Opaque' around the top effect of the effect stack
toOpaque :: Sem (e ': r) a -> Sem (Opaque e ': r) a
toOpaque = coerceEff
{-# INLINE toOpaque #-}

-- | Unwrap 'Opaque' around the top effect of the effect stack
fromOpaque :: Sem (Opaque e ': r) a -> Sem (e ': r) a
fromOpaque = coerceEff
{-# INLINE fromOpaque #-}

toOpaqueAt :: forall l e r a
            . Sem (Append l (e ': r)) a
           -> Sem (Append l (Opaque e ': r)) a
toOpaqueAt = coerceEffAt @l @(Opaque e) @e @r
{-# INLINE toOpaqueAt #-}

fromOpaqueAt :: forall l e r a
              . Sem (Append l (e ': r)) a
             -> Sem (Append l (Opaque e ': r)) a
fromOpaqueAt = coerceEffAt @l @(Opaque e) @e @r
{-# INLINE fromOpaqueAt #-}

-- Length of mid can be specified through type applications when necessary
--
-- TODO: ponder the inconsistency. The length of @l@ is always ambiguous,
-- so no reason not to use ListOfLength, but @mid@ can very well be definite,
-- hence why it's just KnownList, and its length must be provided through
-- @'[_, _, ..., _]. The fact that the length of @l@ is provided through
-- a number and @mid@ is provided through the skeleton of the list is
-- inconsistent and may be unintuitive.
collectOpaqueBundleAt
  :: forall n mid r l a
   . (ListOfLength "collectOpaqueBundleAt" n l, KnownList mid)
  => Sem (Append l (Append mid r)) a
  -> Sem (Append l (Opaque (Bundle mid) ': r)) a
collectOpaqueBundleAt = hoistSem $ \(Union pr wav@(Weaving act mkT lwr ex)) ->
  hoist (collectOpaqueBundleAt @n @mid @r @l)
    case splitMembership @(Append mid r) (singList @l) pr of
      Left pr' ->
        Union (extendMembershipRight @_ @(Opaque (Bundle mid) ': r)
                pr')
              wav
      Right pr' -> case splitMembership @r (singList @mid) pr' of
        Left pr'' ->
          Union (extendMembershipLeft @(Opaque (Bundle mid) ': r)
                  (singList @l) Here)
                (Weaving (Opaque (Bundle pr'' act)) mkT lwr ex)
        Right pr'' ->
          Union (extendMembershipLeft @(Opaque (Bundle mid) ': r)
                  (singList @l)
                  (There pr''))
                wav

runOpaqueBundleAt
  :: forall n mid r l a
   . (ListOfLength "runOpaqueBundleAt" n l, KnownList mid)
  => Sem (Append l (Opaque (Bundle mid) ': r)) a
  -> Sem (Append l (Append mid r)) a
runOpaqueBundleAt = hoistSem $ \(Union pr wav@(Weaving act mkT lwr ex)) ->
  hoist (runOpaqueBundleAt @n @mid @r @l)
    case splitMembership @(Opaque (Bundle mid) ': r) (singList @l) pr of
      Left pr' ->
        Union (extendMembershipRight @_ @(Append mid r) pr') wav
      Right Here | Opaque (Bundle pr' act') <- act ->
        Union (extendMembershipLeft (singList @l)
                (extendMembershipRight @_ @r pr'))
              (Weaving act' mkT lwr ex)
      Right (There pr') ->
        Union (extendMembershipLeft (singList @l)
                (extendMembershipLeft (singList @mid) pr')) wav
