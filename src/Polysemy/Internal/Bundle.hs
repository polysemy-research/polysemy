{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Stack manipulation handlers for the 'Polysemy.Bundle.Bundle' effect
module Polysemy.Internal.Bundle where

import Polysemy (Members)
import Polysemy.Internal.Union (ElemOf(..), membership)
import Polysemy.Internal.Kind (Append)

------------------------------------------------------------------------------
-- | Extend a membership proof's stack by arbitrary effects.
extendMembership :: forall r r' e. ElemOf e r -> ElemOf e (Append r r')
extendMembership Here = Here
extendMembership (There e) = There (extendMembership @_ @r' e)
{-# INLINE extendMembership #-}

------------------------------------------------------------------------------
-- | Transform a membership proof's stack by arbitrary effects using evidence
-- from the context.
subsumeMembership :: forall r r' e. Members r r' => ElemOf e r -> ElemOf e r'
subsumeMembership Here = membership @e @r'
subsumeMembership (There (pr :: ElemOf e r'')) = subsumeMembership @r'' @r' pr
{-# INLINE subsumeMembership #-}
