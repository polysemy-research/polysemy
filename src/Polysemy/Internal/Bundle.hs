{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Stack manipulation handlers for the 'Polysemy.Bundle.Bundle' effect
module Polysemy.Internal.Bundle where

import Polysemy (Members)
import Polysemy.Internal.Union (ElemOf(..), membership)

------------------------------------------------------------------------------
-- | Transform a membership proof's stack by arbitrary effects using evidence
-- from the context.
simpleSubsumeMembership :: forall r r' e. Members r r' => ElemOf e r -> ElemOf e r'
simpleSubsumeMembership Here = membership @e @r'
simpleSubsumeMembership (There (pr :: ElemOf e r'')) = simpleSubsumeMembership @r'' @r' pr
{-# INLINE simpleSubsumeMembership #-}
