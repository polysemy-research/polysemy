-- | Description: Reexports of membership related functionality
module Polysemy.Membership
  ( -- * Witnesses
    ElemOf (..)
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

  -- * Membership manipulation
  , extendMembershipLeft
  , extendMembershipRight
  , injectMembership

    -- * Miscellaneous
  , KnownList(..)
  , SList(..)
  ) where

import Polysemy.Internal
import Polysemy.Internal.Interpret
import Polysemy.Internal.Sing
import Polysemy.Internal.Union
