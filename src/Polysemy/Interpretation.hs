{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}

-- | Everything you need in order to define new effects.
module Polysemy.Interpretation
  ( -- * TH
    makeSemantic
  , makeSemantic_
    -- * First order
  , interpret
  , intercept
  , reinterpret
  , reinterpret2
    -- * Higher order
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
    -- * Statefulness
  , stateful
  , lazilyStateful
    -- * Raising
  , raise
    -- * Performance
  , inlineRecursiveCalls
    -- * Tactics
  , Tactical
  , pureT
  , runT
  , bindT
    -- * Error Messages
  , DefiningModule
  ) where

import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy
import           Polysemy.Internal.Effect
import           Polysemy.Internal.TH.Effect
import           Polysemy.Internal.TH.Performance
import           Polysemy.Internal.Tactics
import           Polysemy.Internal.Union
import           Polysemy.Union.TypeErrors


------------------------------------------------------------------------------
-- | A lazier version of 'Data.Tuple.swap'.
swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)



interpret
    :: FirstOrder e "interpret"
    => (∀ x m. e m x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
-- TODO(sandy): could probably give a `coerce` impl for `runTactics` here
interpret f = interpretH $ \(e :: e m x) -> liftT @m $ f e

interpretH
    :: forall e r a
     . (∀ x m . e m x -> Tactical e m r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpretH f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpretH_b f) x
    Right (Yo e s d y) -> do
      a <- runTactics s (raise . interpretH_b f . d) (f e)
      pure $ y a
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: (∀ x m. e m x -> S.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInStateT_b f)
            $ x
        Right (Yo e z _ y) ->
          fmap (y . (<$ z)) $ S.mapStateT (usingSemantic k) $ f e
{-# INLINE interpretInStateT #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInLazyStateT
    :: (∀ x m. e m x -> LS.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInLazyStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip LS.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> LS.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInLazyStateT_b f)
            $ x
        Right (Yo e z _ y) ->
          fmap (y . (<$ z)) $ LS.mapStateT (usingSemantic k) $ f e
{-# INLINE interpretInLazyStateT #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: (∀ x m. e m x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ fmap swap . f e
{-# INLINE[3] stateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
lazilyStateful
    :: (∀ x m. e m x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap swap . f e
{-# INLINE[3] lazilyStateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- TODO(sandy): Make this fuse in with 'stateful' directly.
reinterpretH
    :: forall e2 e1 r a
     . (∀ m x. e1 m x -> Tactical e1 m (e2 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': r) a
reinterpretH f (Semantic m) = Semantic $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ hoist (reinterpretH_b f) $ x
    Right (Yo e s d y) -> do
      a <- usingSemantic k $ runTactics s (raise . reinterpretH_b f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpretH #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- TODO(sandy): Make this fuse in with 'stateful' directly.
reinterpret
    :: forall e2 e1 r a
     . FirstOrder e1 "reinterpret"
    => (∀ m x. e1 m x -> Semantic (e2 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': r) a
reinterpret f = reinterpretH $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE[3] reinterpret #-}

reinterpret2H
    :: forall e2 e3 e1 r a
     . (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': e3 ': r) a
reinterpret2H f (Semantic m) = Semantic $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ weaken $ hoist (reinterpret2H_b f) $ x
    Right (Yo e s d y) -> do
      a <- usingSemantic k $ runTactics s (raise . reinterpret2H_b f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpret2H #-}

reinterpret2
    :: forall e2 e3 e1 r a
     . FirstOrder e1 "reinterpret2"
    => (∀ m x. e1 m x -> Semantic (e2 ': e3 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': e3 ': r) a
reinterpret2 f = reinterpret2H $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE[3] reinterpret2 #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
intercept
    :: forall e r a
     . ( Member e r
       , FirstOrder e "intercept"
       )
    => (∀ x m. e m x -> Semantic r x)
    -> Semantic r a
    -> Semantic r a
intercept f = interceptH $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE intercept #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
interceptH
    :: forall e r a. Member e r
    => (∀ x m. e m x -> Tactical e m r x)
    -> Semantic r a
    -> Semantic r a
interceptH f (Semantic m) = Semantic $ \k -> m $ \u ->
  case prj @e u of
    Just (Yo e s d y) ->
      usingSemantic k $ fmap y $ runTactics s (raise . d) $ f e
    Nothing -> k u
{-# INLINE interceptH #-}

------------------------------------------------------------------------------
-- Loop breakers
interpretH_b
    :: forall e r a
     . (∀ x m . e m x -> Tactical e m r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpretH_b = interpretH
{-# NOINLINE interpretH_b #-}

interpretInStateT_b
    :: (∀ x m. e m x -> S.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT_b = interpretInStateT
{-# NOINLINE interpretInStateT_b #-}

interpretInLazyStateT_b
    :: (∀ x m. e m x -> LS.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInLazyStateT_b = interpretInLazyStateT
{-# NOINLINE interpretInLazyStateT_b #-}

reinterpretH_b
    :: forall e2 e1 r a
     . (∀ m x. e1 m x -> Tactical e1 m (e2 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': r) a
reinterpretH_b = reinterpretH
{-# NOINLINE reinterpretH_b #-}

reinterpret2H_b
    :: forall e2 e3 e1 r a
     . (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': e3 ': r) a
reinterpret2H_b = reinterpret2H
{-# NOINLINE reinterpret2H_b #-}

