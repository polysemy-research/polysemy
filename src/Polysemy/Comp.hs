{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.Comp where

import Data.Proxy
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
-- import Data.Type.Equality


data Comp (r :: EffectRow) m a where
  Comp :: EffectOf r e -> e m a -> Comp r m a

sendComp :: forall e r' r a
          . (Member e r', Member (Comp r') r)
         => Sem (e ': r) a
         -> Sem r a
sendComp = hoistSem $ \u -> case decomp u of
  Right (Weaving e s wv ex ins) ->
    injWeaving $
      Weaving (Comp (membership @e @r') e) s (sendComp @e @r' . wv) ex ins
  Left g -> hoist (sendComp @e @r') g

type family Append l r where
  Append (a ': l) r = a ': (Append l r)
  Append '[] r = r

runComp :: forall r' r a
         . KnownList r'
        => Sem (Comp r' ': r) a
        -> Sem (Append r' r) a
runComp = hoistSem $ \u -> hoist runComp $ case decomp u of
  Right (Weaving (Comp pr e) s wv ex ins) ->
    Union (extendMembership @_ @r pr) $ Weaving e s wv ex ins
  Left g -> weakenList @r' @r g


subsumeMembership :: forall r r' e. Members r r' => EffectOf r e -> EffectOf r' e
subsumeMembership Choice = membership @e @r'
subsumeMembership (Other (pr :: EffectOf r'' e)) = subsumeMembership @r'' @r' pr

subsumeComp :: forall r' r a
             . Members r' r
            => Sem (Comp r' ': r) a
            -> Sem r a
subsumeComp = hoistSem $ \u -> hoist subsumeComp $ case decomp u of
  Right (Weaving (Comp pr e) s wv ex ins) ->
    Union (subsumeMembership pr) (Weaving e s wv ex ins)
  Left g -> g

extendMembership :: forall r r' e. EffectOf r e -> EffectOf (Append r r') e
extendMembership Choice = Choice
extendMembership (Other e) = Other (extendMembership @_ @r' e)

-- data Comp' (r :: EffectRow) m a where
--   Comp' :: SNat n -> IndexOf r n m a -> Comp' r m a

-- type family Append l r where
--   Append (a ': l) r = a ': (Append l r)
--   Append '[] r = r


-- injComp' :: forall e r' r a
--           . (Member e r', Member (Comp' r') r)
--          => Sem (e ': r) a
--          -> Sem r a
-- injComp' = hoistSem $ \u -> case decomp u of
--   Right (Weaving e s wv ex ins) ->
--     injWeaving @(Comp' r') $
--       Weaving (Comp' (finder @_ @r' @e) e) s (injComp' @e @r' . wv) ex ins
--   Left g -> hoist (injComp' @e @r') g

-- {-



-- -}

-- injComp :: e ~ IndexOf r (Found r e) => SNat (Found r e) -> e m a -> Comp r m a
-- injComp SZ e = Choice e
-- injComp (SS n) e = Other (injComp n e)

-- sendComp :: forall e r' r a
--           . (Member e r', Member (Comp r') r)
--          => Sem (e ': r) a
--          -> Sem r a
-- sendComp = hoistSem $ \u -> case decomp u of
--   Right (Weaving e s wv ex ins) -> undefined
  

-- appendComp :: Weaving (Comp r') (Sem (e ': r)) a
--            -> Union (Append r' r) (Sem (e ': r)) a
-- appendComp (Weaving (Choice e) s wv ex ins)
--   = Union SZ (Weaving e s wv ex ins)
-- appendComp (Weaving (Other e) s wv ex ins)
--   = case appendComp (Weaving e s wv ex ins) of
--     Union n wav -> Union (SS n) wav

-- injectComp :: Members r' r
--            => Weaving (Comp r') (Sem (e ': r)) a
--            -> Union r (Sem (e ': r)) a
-- injectComp (Weaving (Choice e) s wv ex ins) =
--   injWeaving (Weaving e s wv ex ins)
-- injectComp (Weaving (Other e) s wv ex ins) =
--   injectComp (Weaving e s wv ex ins)


weakenList :: forall r' r m a
            . KnownList r'
           => Union r m a
           -> Union (Append r' r) m a
weakenList u = unconsKnownList @_ @r' u (\_ (_ :: Proxy r'') -> weaken (weakenList @r'' u))

-- runComp :: forall r' r a
--          . KnownList r'
--         => Sem (Comp r' ': r) a
--         -> Sem (Append r' r) a
-- runComp = hoistSem $ \u -> hoist runComp $ case decomp u of
--   Right wav -> appendComp wav
--   Left g    -> weakenList @r' g

-- subsumeComp :: Members r' r
--             => Sem (Comp r' ': r) a
--             -> Sem r a
-- subsumeComp = hoistSem $ \u -> hoist subsumeComp $ case decomp u of
--   Right wav -> injectComp wav
--   Left  g   -> g

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
