-- | Everything you need in order to define new effects.
module Polysemy.Effect.New
  ( Effect (..)
  , defaultHoist
  , makeSemantic
  , makeSemantic_
  , interpret
  , intercept
  , reinterpret
  , stateful
  , lazilyStateful
  , interpretInLazyStateT
  , Union ()
  , decomp
  , prj
  , decompCoerce
  , weaken
  , inlineRecursiveCalls
  ) where

import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.State.Lazy as LS
import           Polysemy
import           Polysemy.Effect
import           Polysemy.Effect.TH
import           Polysemy.Performance.TH
import           Polysemy.Union


------------------------------------------------------------------------------
-- | A lazier version of 'Data.Tuple.swap'.
swap :: (a,b) -> (b,a)
swap ~(a, b) = (b,a)


------------------------------------------------------------------------------
-- | Interpret an effect by rewriting it in terms of capabilities already
-- present in the underlying effect stack.
interpret
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpret f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (___interpret___loop_breaker f) x
    Right y -> f y
{-# INLINE interpret #-}


------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ ___interpretInStateT___loop_breaker f)
            $ x
        Right y -> S.mapStateT (usingSemantic k) $ f y
{-# INLINE interpretInStateT #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInLazyStateT
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> LS.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInLazyStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip LS.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> LS.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ ___interpretInLazyStateT___loop_breaker f)
            $ x
        Right y -> LS.mapStateT (usingSemantic k) $ f y
{-# INLINE interpretInLazyStateT #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- TODO(sandy): Make this fuse in with 'stateful' directly.
reinterpret
    :: forall f e r a
     . Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic (f ': r) x)
    -> Semantic (e ': r) a
    -> Semantic (f ': r) a
reinterpret f (Semantic m) = Semantic $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ hoist (___reinterpret___loop_breaker f) $ x
    Right y -> usingSemantic k $ f y
{-# INLINE[3] reinterpret #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ fmap swap . f e
{-# INLINE[3] stateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
lazilyStateful
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap swap . f e
{-# INLINE[3] lazilyStateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
intercept
    :: forall e r a. Member e r
    => (∀ x. e (Semantic r) x -> Semantic r x)
    -> Semantic r a
    -> Semantic r a
intercept f (Semantic m) = Semantic $ \k -> m $ \u ->
  case prj @e u of
    Just e  -> usingSemantic k $ f e
    Nothing -> k u
{-# INLINE intercept #-}


------------------------------------------------------------------------------
-- | Explicit loop breakers so that GHC will properly inline all of the above.
___interpret___loop_breaker
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
___interpret___loop_breaker = interpret
{-# NOINLINE ___interpret___loop_breaker #-}

___interpretInStateT___loop_breaker
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
___interpretInStateT___loop_breaker = interpretInStateT
{-# NOINLINE ___interpretInStateT___loop_breaker #-}

___interpretInLazyStateT___loop_breaker
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> LS.StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
___interpretInLazyStateT___loop_breaker = interpretInLazyStateT
{-# NOINLINE ___interpretInLazyStateT___loop_breaker #-}

___reinterpret___loop_breaker
    :: forall f e r a
     . Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic (f ': r) x)
    -> Semantic (e ': r) a
    -> Semantic (f ': r) a
___reinterpret___loop_breaker = reinterpret
{-# NOINLINE ___reinterpret___loop_breaker #-}

