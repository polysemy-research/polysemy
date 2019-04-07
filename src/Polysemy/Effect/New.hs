-- | Everything you need in order to define new effects.
module Polysemy.Effect.New
  ( Effect (..)
  , defaultHoist
  , makeSemantic
  , makeSemantic_
  , interpret
  , interpretH
  , reinterpret
  , reinterpretH
  , reinterpret2
  , reinterpret2H
  , intercept
  , stateful
  , lazilyStateful
  , Union ()
  , decomp
  , prj
  , decompCoerce
  , weaken
  , inlineRecursiveCalls
  ) where

import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy
import           Polysemy.Effect
import           Polysemy.Effect.TH
import           Polysemy.Performance.TH
import           Polysemy.Tactics.Type
import           Polysemy.Union


------------------------------------------------------------------------------
-- | A lazier version of 'Data.Tuple.swap'.
swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)


type Tactically m r x = ∀ f. Functor f => Semantic (Tactics f m r ': r) (f x)

interpret
    :: (∀ x m. e m x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
-- TODO(sandy): could probably give a `coerce` impl for `runTactics` here
interpret f = interpretH $ \(e :: e m x) -> toH @m $ f e

interpretH
    :: forall e r a
     . (∀ x m . e m x -> Tactically m r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpretH f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpretH f) x
    Right (Yo e s d y) -> do
      a <- runTactics s (interpretH f . d) (f e)
      pure $ y a
{-# INLINE interpretH #-}


-- data Bracket m a where
--   Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Bracket m b

-- runBracket
--     :: Member (Lift IO) r
--     => (∀ x. Semantic r x -> IO x)
--     -> Semantic (Bracket ': r) a
--     -> Semantic r a
-- runBracket lower = interpretH $ \case
--   Bracket open close use -> do
--     o <- start    open
--     c <- continue close
--     u <- continue use
--     sendM $ X.bracket (lower o) (lower . c) (lower . u)

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
     . (∀ m x. e1 m x -> Tactically m (e2 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': r) a
reinterpretH f (Semantic m) = Semantic $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ hoist (reinterpretH f) $ x
    Right (Yo e s d y) -> do
      a <- usingSemantic k $ runTactics s (reinterpretH f . d) $ f e
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
     . (∀ m x. e1 m x -> Semantic (e2 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': r) a
reinterpret f = reinterpretH $ \(e :: e m x) -> toH @m $ f e
{-# INLINE[3] reinterpret #-}

reinterpret2H
    :: forall e2 e3 e1 r a
     . (∀ m x. e1 m x -> Tactically m (e2 ': e3 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': e3 ': r) a
reinterpret2H f (Semantic m) = Semantic $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ weaken $ hoist (reinterpret2H f) $ x
    Right (Yo e s d y) -> do
      a <- usingSemantic k $ runTactics s (reinterpret2H f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpret2H #-}

reinterpret2
    :: forall e2 e3 e1 r a
     . (∀ m x. e1 m x -> Semantic (e2 ': e3 ': r) x)
    -> Semantic (e1 ': r) a
    -> Semantic (e2 ': e3 ': r) a
reinterpret2 f = reinterpret2H $ \(e :: e m x) -> toH @m $ f e
{-# INLINE[3] reinterpret2 #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled.
intercept
    :: forall e r a. Member e r
    => (∀ x m. e m x -> Semantic r x)
    -> Semantic r a
    -> Semantic r a
intercept f (Semantic m) = Semantic $ \k -> m $ \u ->
  case prj @e u of
    Just (Yo e s _ y) -> usingSemantic k $ fmap (y . (<$ s)) $ f e
    Nothing -> k u
{-# INLINE intercept #-}

