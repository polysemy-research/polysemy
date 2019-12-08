{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Bundle where

import Data.Proxy
import Polysemy
import Polysemy.Internal.Union

type family Append l r where
  Append (a ': l) r = a ': (Append l r)
  Append '[] r = r

extendMembership :: forall r r' e. ElemOf e r -> ElemOf e (Append r r')
extendMembership Here = Here
extendMembership (There e) = There (extendMembership @_ @r' e)
{-# INLINE extendMembership #-}

subsumeMembership :: forall r r' e. Members r r' => ElemOf e r -> ElemOf e r'
subsumeMembership Here = membership @e @r'
subsumeMembership (There (pr :: ElemOf e r'')) = subsumeMembership @r'' @r' pr
{-# INLINE subsumeMembership #-}

weakenList :: forall r' r m a
            . KnownList r'
           => Union r m a
           -> Union (Append r' r) m a
weakenList u = unconsKnownList @_ @r' u (\_ (_ :: Proxy r'') -> weaken (weakenList @r'' u))
{-# INLINE weakenList #-}


------------------------------------------------------------------------------
-- | A class for type-level lists with a known spine.
--
-- This constraint is eventually satisfied as @r@ is instantied to a
-- concrete list.
class KnownList (l :: [k]) where
  unconsKnownList :: (l ~ '[] => a)
                  -> (  forall x r
                      . (l ~ (x ': r), KnownList r)
                     => Proxy x
                     -> Proxy r
                     -> a
                     )
                  -> a

instance KnownList '[] where
  unconsKnownList b _ = b
  {-# INLINE unconsKnownList #-}

instance KnownList r => KnownList (x ': r) where
  unconsKnownList _ b = b Proxy Proxy
  {-# INLINE unconsKnownList #-}

