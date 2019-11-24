{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Compose
  ( Compose (..)
  , sendCompose
  , runCompose
  , subsumeCompose
  , KnownList
  ) where

import Data.Proxy
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union

data Compose :: EffectRow -> Effect where
  Compose :: ElemOf r e -> e m a -> Compose r m a

sendCompose
  :: forall e r' r a
   . (Member e r', Member (Compose r') r)
  => Sem (e ': r) a
  -> Sem r a
sendCompose = hoistSem $ \u -> case decomp u of
  Right (Weaving e s wv ex ins) ->
    injWeaving $
      Weaving (Compose (membership @e @r') e) s (sendCompose @e @r' . wv) ex ins
  Left g -> hoist (sendCompose @e @r') g

type family Append l r where
  Append (a ': l) r = a ': (Append l r)
  Append '[] r = r

runCompose
  :: forall r' r a
   . KnownList r'
  => Sem (Compose r' ': r) a
  -> Sem (Append r' r) a
runCompose = hoistSem $ \u -> hoist runCompose $ case decomp u of
  Right (Weaving (Compose pr e) s wv ex ins) ->
    Union (extendMembership @_ @r pr) $ Weaving e s wv ex ins
  Left g -> weakenList @r' @r g


subsumeMembership :: forall r r' e. Members r r' => ElemOf r e -> ElemOf r' e
subsumeMembership Here = membership @e @r'
subsumeMembership (In (pr :: ElemOf r'' e)) = subsumeMembership @r'' @r' pr

subsumeCompose
  :: forall r' r a
   . Members r' r
  => Sem (Compose r' ': r) a
  -> Sem r a
subsumeCompose = hoistSem $ \u -> hoist subsumeCompose $ case decomp u of
  Right (Weaving (Compose pr e) s wv ex ins) ->
    Union (subsumeMembership pr) (Weaving e s wv ex ins)
  Left g -> g

extendMembership :: forall r r' e. ElemOf r e -> ElemOf (Append r r') e
extendMembership Here = Here
extendMembership (In e) = In (extendMembership @_ @r' e)

weakenList :: forall r' r m a
            . KnownList r'
           => Union r m a
           -> Union (Append r' r) m a
weakenList u = unconsKnownList @_ @r' u (\_ (_ :: Proxy r'') -> weaken (weakenList @r'' u))

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

instance KnownList r => KnownList (x ': r) where
  unconsKnownList _ b = b Proxy Proxy
