{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Polysemy.Internal.Combinators
  ( -- * First order
    interpret
  , intercept
  , reinterpret
  , reinterpret2
  , reinterpret3
    -- * Higher order
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
  , reinterpret3H
    -- * Statefulness
  , stateful
  , lazilyStateful
  ) where

import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy.Internal
import           Polysemy.Internal.CustomErrors
import           Polysemy.Internal.Effect
import           Polysemy.Internal.Tactics
import           Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | A lazier version of 'Data.Tuple.swap'.
swap :: (a, b) -> (b, a)
swap ~(a, b) = (b, a)



------------------------------------------------------------------------------
-- | The simplest way to produce an effect handler. Interprets an effect @e@ by
-- transforming it into other effects inside of @r@.
interpret
    :: FirstOrder e "interpret"
    => (∀ x m. e m x -> Sem r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem (e ': r) a
    -> Sem r a
-- TODO(sandy): could probably give a `coerce` impl for `runTactics` here
interpret f = interpretH $ \(e :: e m x) -> liftT @m $ f e


------------------------------------------------------------------------------
-- | Like 'interpret', but for higher-order effects (ie. those which make use of
-- the @m@ parameter.)
--
-- See the notes on 'Tactical' for how to use this function.
interpretH
    :: (∀ x m . e m x -> Tactical e m r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem (e ': r) a
    -> Sem r a
interpretH f (Sem m) = m $ \u ->
  case decomp u of
    Left  x -> liftSem $ hoist (interpretH_b f) x
    Right (Yo e s d y) -> do
      a <- runTactics s (raise . interpretH_b f . d) (f e)
      pure $ y a
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInStateT f s (Sem m) = Sem $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInStateT_b f)
            $ x
        Right (Yo e z _ y) ->
          fmap (y . (<$ z)) $ S.mapStateT (usingSem k) $ f e
{-# INLINE interpretInStateT #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInLazyStateT
    :: (∀ x m. e m x -> LS.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInLazyStateT f s (Sem m) = Sem $ \k ->
  fmap swap $ flip LS.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> LS.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInLazyStateT_b f)
            $ x
        Right (Yo e z _ y) ->
          fmap (y . (<$ z)) $ LS.mapStateT (usingSem k) $ f e
{-# INLINE interpretInLazyStateT #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ fmap swap . f e
{-# INLINE[3] stateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
lazilyStateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap swap . f e
{-# INLINE[3] lazilyStateful #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but for higher-order effects.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpretH
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effect.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
reinterpretH f (Sem m) = Sem $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ hoist (reinterpretH_b f) $ x
    Right (Yo e s d y) -> do
      a <- usingSem k $ runTactics s (raise . reinterpretH_b f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpretH #-}
-- TODO(sandy): Make this fuse in with 'stateful' directly.


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
reinterpret
    :: FirstOrder e1 "reinterpret"
    => (∀ m x. e1 m x -> Sem (e2 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effect.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
reinterpret f = reinterpretH $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE[3] reinterpret #-}
-- TODO(sandy): Make this fuse in with 'stateful' directly.


------------------------------------------------------------------------------
-- | Like 'reinterpret2', but for higher-order effects.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpret2H
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': r) a
reinterpret2H f (Sem m) = Sem $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ weaken $ hoist (reinterpret2H_b f) $ x
    Right (Yo e s d y) -> do
      a <- usingSem k $ runTactics s (raise . reinterpret2H_b f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpret2H #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /two/ intermediary effects.
reinterpret2
    :: FirstOrder e1 "reinterpret2"
    => (∀ m x. e1 m x -> Sem (e2 ': e3 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': r) a
reinterpret2 f = reinterpret2H $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE[3] reinterpret2 #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret3', but for higher-order effects.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpret3H
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': e4 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3H f (Sem m) = Sem $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k . weaken . weaken . hoist (reinterpret3H_b f) $ x
    Right (Yo e s d y) -> do
      a <- usingSem k $ runTactics s (raise . reinterpret3H_b f . d) $ f e
      pure $ y a
{-# INLINE[3] reinterpret3H #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /three/ intermediary effects.
reinterpret3
    :: FirstOrder e1 "reinterpret3"
    => (∀ m x. e1 m x -> Sem (e2 ': e3 ': e4 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3 f = reinterpret3H $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE[3] reinterpret3 #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled. This allows you, for example, to
-- intercept other effects and insert logic around them.
intercept
    :: ( Member e r
       , FirstOrder e "intercept"
       )
    => (∀ x m. e m x -> Sem r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpret', 'intercept' does not consume any effects.
    -> Sem r a
intercept f = interceptH $ \(e :: e m x) -> liftT @m $ f e
{-# INLINE intercept #-}


------------------------------------------------------------------------------
-- | Like 'interceptH', but for higher-order effects.
--
-- See the notes on 'Tactical' for how to use this function.
interceptH
    :: Member e r
    => (∀ x m. e m x -> Tactical e m r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpretH', 'interceptH' does not consume any effects.
    -> Sem r a
interceptH f (Sem m) = Sem $ \k -> m $ \u ->
  case prj u of
    Just (Yo e s d y) ->
      usingSem k $ fmap y $ runTactics s (raise . d) $ f e
    Nothing -> k u
{-# INLINE interceptH #-}


------------------------------------------------------------------------------
-- Loop breakers
interpretH_b
    :: (∀ x m . e m x -> Tactical e m r x)
    -> Sem (e ': r) a
    -> Sem r a
interpretH_b = interpretH
{-# NOINLINE interpretH_b #-}


interpretInStateT_b
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInStateT_b = interpretInStateT
{-# NOINLINE interpretInStateT_b #-}


interpretInLazyStateT_b
    :: (∀ x m. e m x -> LS.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInLazyStateT_b = interpretInLazyStateT
{-# NOINLINE interpretInLazyStateT_b #-}


reinterpretH_b
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': r) x)
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
reinterpretH_b = reinterpretH
{-# NOINLINE reinterpretH_b #-}


reinterpret2H_b
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': r) x)
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': r) a
reinterpret2H_b = reinterpret2H
{-# NOINLINE reinterpret2H_b #-}


reinterpret3H_b
    :: (∀ m x. e1 m x -> Tactical e1 m (e2 ': e3 ': e4 ': r) x)
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3H_b = reinterpret3H
{-# NOINLINE reinterpret3H_b #-}

