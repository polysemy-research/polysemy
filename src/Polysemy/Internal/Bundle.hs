{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Bundle where

import Polysemy (Members)
import Polysemy.Internal.Union (ElemOf(..), membership)
import Polysemy.Internal.Kind (Append)

extendMembership :: forall r r' e. ElemOf e r -> ElemOf e (Append r r')
extendMembership Here = Here
extendMembership (There e) = There (extendMembership @_ @r' e)
{-# INLINE extendMembership #-}

subsumeMembership :: forall r r' e. Members r r' => ElemOf e r -> ElemOf e r'
subsumeMembership Here = membership @e @r'
subsumeMembership (There (pr :: ElemOf e r'')) = subsumeMembership @r'' @r' pr
{-# INLINE subsumeMembership #-}
