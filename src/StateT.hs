{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module StateT
  ( module StateT
  , module Control.Monad.State.Class
  ) where

import Data.Tuple
import Control.Monad.Trans.Class
import Control.Monad.Fix
import Control.Monad.State.Class

newtype StateT s m a = StateT
  { runStateT :: s -> m (s, a)
  }
  deriving Functor


-- | Evaluate a state computation with the given initial state
-- and return the final value, discarding the final state.
--
-- * @'evalStateT' m s = 'liftM' 'fst' ('runStateT' m s)@
evalStateT :: (Monad m) => StateT s m a -> s -> m a
evalStateT m s = do
    (_, a) <- runStateT m s
    return a
{-# INLINE evalStateT #-}

-- | Evaluate a state computation with the given initial state
-- and return the final state, discarding the final value.
--
-- * @'execStateT' m s = 'liftM' 'snd' ('runStateT' m s)@
execStateT :: (Monad m) => StateT s m a -> s -> m s
execStateT m s = do
    (s', _) <- runStateT m s
    return s'
{-# INLINE execStateT #-}

-- | Map both the return value and final state of a computation using
-- the given function.
--
-- * @'runStateT' ('mapStateT' f m) = f . 'runStateT' m@
mapStateT :: (m (s, a) -> n (s, b)) -> StateT s m a -> StateT s n b
mapStateT f m = StateT $ f . runStateT m
{-# INLINE mapStateT #-}

instance (Functor m, Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \ s -> return (s, a)
    {-# INLINE pure #-}
    StateT mf <*> StateT mx = StateT $ \ s -> do
        (s', f) <- mf s
        (s'', x) <- mx s'
        return (s'', f x)
    {-# INLINE (<*>) #-}
    m *> k = m >>= \_ -> k
    {-# INLINE (*>) #-}

instance (Monad m) => Monad (StateT s m) where
    m >>= k  = StateT $ \ s -> do
        (s', a) <- runStateT m s
        runStateT (k a) s'
    {-# INLINE (>>=) #-}

instance (MonadFix m) => MonadFix (StateT s m) where
    mfix f = StateT $ \ s -> mfix $ \ ~(_, a) -> runStateT (f a) s
    {-# INLINE mfix #-}

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> do
        a <- m
        return (s, a)
    {-# INLINE lift #-}

instance Monad m => MonadState s (StateT s m) where
  get   = StateT $ \s -> pure (s, s)
  put s = StateT $ \_ -> pure (s, ())
  state = StateT . fmap (pure . swap)

