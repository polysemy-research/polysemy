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
  ) where

import Polysemy.Internal
import Polysemy.Internal.Union
