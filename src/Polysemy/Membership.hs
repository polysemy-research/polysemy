-- | Description: Reexports of membership related functionality
module Polysemy.Membership
  ( -- * Witnesses
    ElemOf (Here, There)
  , membership
  , sameMember

  -- * Checking membership
  , KnownRow
  , tryMembership

  -- * Using membership
  , subsumeUsing
  , exposeUsing
  , interceptUsing
  , interceptUsingH
  , mapMembership

  -- * Membership manipulation
  , extendMembershipLeft
  , extendMembershipRight
  , injectMembership

    -- * Miscellaneous
  , KnownList(..)
  , SList(SEnd, SCons)
  ) where

import Polysemy.Internal
import Polysemy.Internal.HigherOrder
import Polysemy.Internal.Sing
import Polysemy.Internal.Union
