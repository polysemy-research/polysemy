-- | Everything you need in order to define new effects.
module Polysemy.Effect.New
  ( Effect (..)
  , defaultHoist
  , makeSemantic
  , makeSemantic_
  , interpret
  , interpretH
  -- , intercept
  -- , reinterpret
  -- , stateful
  -- , lazilyStateful
  , interpretInLazyStateT
  , Union ()
  , decomp
  , prj
  , decompCoerce
  , weaken
  , inlineRecursiveCalls
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Trans.State.Lazy as LS
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy
import           Polysemy.Effect
import           Polysemy.Effect.TH
import           Polysemy.Performance.TH
import           Polysemy.Tactics.Type
import           Polysemy.Union


------------------------------------------------------------------------------
-- | A lazier version of 'Data.Tuple.swap'.
swap :: (a,b) -> (b,a)
swap ~(a, b) = (b,a)


------------------------------------------------------------------------------
-- | Interpret an effect by rewriting it in terms of capabilities already
-- present in the underlying effect stack.
interpret
    :: (∀ x m. e m x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpret f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpret f) x
    Right (Yo e s _ y) -> do
      a <- f e
      pure $ y $ a <$ s
{-# INLINE interpret #-}

data Bracket m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Bracket m b

runBracket
    :: Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Bracket ': r) a
    -> Semantic r a
runBracket lower = interpretH $ \case
  Bracket open close use -> do
    o <- start    open
    c <- continue close
    u <- continue use
    sendM $ X.bracket (lower o) (lower . c) (lower . u)


interpretH
    :: forall e r a
     . (∀ x m f . e m x -> Semantic (Tactics f m r ': r) (f x))
    -> Semantic (e ': r) a
    -> Semantic r a
interpretH f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpretH f) x
    Right (Yo e s d y) -> do
      a <- runTactics s (interpretH f . d) (f e)
      pure $ y a
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: (∀ x m. e m x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInStateT f)
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
            . weave (s', ()) (uncurry $ interpretInLazyStateT f)
            $ x
        Right (Yo e z _ y) ->
          fmap (y . (<$ z)) $ LS.mapStateT (usingSemantic k) $ f e
{-# INLINE interpretInLazyStateT #-}


--------------------------------------------------------------------------------
---- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
---- some new effect @f@. This function will fuse when followed by
---- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
---- the 'Polysemy.State.State' effect and immediately run it.
----
---- TODO(sandy): Make this fuse in with 'stateful' directly.
--reinterpret
--    :: forall f e r a
--     . Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> Semantic (f ': r) x)
--    -> Semantic (e ': r) a
--    -> Semantic (f ': r) a
--reinterpret f (Semantic m) = Semantic $ \k -> m $ \u ->
--  case decompCoerce u of
--    Left x  -> k $ hoist (___reinterpret___loop_breaker f) $ x
--    Right y -> usingSemantic k $ f y
--{-# INLINE[3] reinterpret #-}


--------------------------------------------------------------------------------
---- | Like 'interpret', but with access to an intermediate state @s@.
--stateful
--    :: Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
--    -> s
--    -> Semantic (e ': r) a
--    -> Semantic r (s, a)
--stateful f = interpretInStateT $ \e -> S.StateT $ fmap swap . f e
--{-# INLINE[3] stateful #-}


--------------------------------------------------------------------------------
---- | Like 'interpret', but with access to an intermediate state @s@.
--lazilyStateful
--    :: Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
--    -> s
--    -> Semantic (e ': r) a
--    -> Semantic r (s, a)
--lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap swap . f e
--{-# INLINE[3] lazilyStateful #-}


--------------------------------------------------------------------------------
---- | Like 'interpret', but instead of handling the effect, allows responding to
---- the effect while leaving it unhandled.
--intercept
--    :: forall e r a. Member e r
--    => (∀ x. e (Semantic r) x -> Semantic r x)
--    -> Semantic r a
--    -> Semantic r a
--intercept f (Semantic m) = Semantic $ \k -> m $ \u ->
--  case prj @e u of
--    Just e  -> usingSemantic k $ f e
--    Nothing -> k u
--{-# INLINE intercept #-}


--------------------------------------------------------------------------------
---- | Explicit loop breakers so that GHC will properly inline all of the above.
--___interpret___loop_breaker
--    :: Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> Semantic r x)
--    -> Semantic (e ': r) a
--    -> Semantic r a
--___interpret___loop_breaker = interpret
--{-# NOINLINE ___interpret___loop_breaker #-}

--___interpretInStateT___loop_breaker
--    :: Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
--    -> s
--    -> Semantic (e ': r) a
--    -> Semantic r (s, a)
--___interpretInStateT___loop_breaker = interpretInStateT
--{-# NOINLINE ___interpretInStateT___loop_breaker #-}

--___interpretInLazyStateT___loop_breaker
--    :: Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> LS.StateT s (Semantic r) x)
--    -> s
--    -> Semantic (e ': r) a
--    -> Semantic r (s, a)
--___interpretInLazyStateT___loop_breaker = interpretInLazyStateT
--{-# NOINLINE ___interpretInLazyStateT___loop_breaker #-}

--___reinterpret___loop_breaker
--    :: forall f e r a
--     . Effect e
--    => (∀ x. e (Semantic (e ': r)) x -> Semantic (f ': r) x)
--    -> Semantic (e ': r) a
--    -> Semantic (f ': r) a
--___reinterpret___loop_breaker = reinterpret
--{-# NOINLINE ___reinterpret___loop_breaker #-}

