{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Compose where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union

data Compose e e' m a where
  LeftE  :: forall e e' m a. e m a -> Compose e e' m a
  RightE :: forall e e' m a. e' m a -> Compose e e' m a

sendLeft :: forall e e' r a
          . Member (Compose e e') r
         => Sem (e ': r) a
         -> Sem r a
sendLeft = hoistSem $ \u -> hoist (sendLeft @e @e') $ case decomp u of
  Right (Weaving e s wv ex ins) ->
    injWeaving $ Weaving (LeftE @e @e' e) s wv ex ins
  Left g ->
    g

runCompose :: Sem (Compose e e' ': r) a
           -> Sem (e ': e' ': r) a
runCompose = hoistSem $ \u -> hoist runCompose $ case decompCoerce u of
  Right (Weaving e s wv ex ins) -> case e of
    LeftE e' -> Union Choice (Weaving e' s wv ex ins)
    RightE e' -> Union (Other Choice) (Weaving e' s wv ex ins)
  Left g -> weaken g
