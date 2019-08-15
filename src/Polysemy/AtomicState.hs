{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
module Polysemy.AtomicState
  ( -- * Effect
    AtomicState (..)

    -- * Actions
  , atomicState
  , atomicState'
  , atomicGet
  , atomicPut
  , atomicModify
  , atomicModify'

    -- * Interpretations
  , runAtomicStateIORef
  , runAtomicStateTVar
  , atomicStateToState
  ) where


import Control.Concurrent.STM

import Polysemy
import Polysemy.State

import Data.IORef

data AtomicState s m a where
  AtomicState :: (s -> (s, a)) -> AtomicState s m a
  AtomicGet   :: AtomicState s m s

makeSem_ ''AtomicState

-----------------------------------------------------------------------------
-- | Atomically reads and modifies the state.
atomicState :: forall s a r
             . Member (AtomicState s) r
            => (s -> (s, a))
            -> Sem r a

atomicGet :: forall s r
           . Member (AtomicState s) r
          => Sem r s

-----------------------------------------------------------------------------
-- | A variant of 'atomicState' in which the computation is strict in the new
-- state and return value.
atomicState' :: Member (AtomicState s) r
              => (s -> (s, a))
              -> Sem r a
atomicState' f = do
  -- KingoftheHomeless: return value needs to be forced due to how
  -- 'atomicModifyIORef' is implemented: the computation
  -- (and thus the new state) is forced only once the return value is.
  !a <- atomicState $ \s ->
    case f s of
      v@(!_, _) -> v
  return a
{-# INLINE atomicState' #-}

atomicPut :: Member (AtomicState s) r
          => s
          -> Sem r ()
atomicPut s = do
  !_ <- atomicState $ \_ -> (s, ()) -- strict put with atomicModifyIORef
  return ()
{-# INLINE atomicPut #-}

atomicModify :: Member (AtomicState s) r
             => (s -> s)
             -> Sem r ()
atomicModify f = atomicState $ \s -> (f s, ())
{-# INLINE atomicModify #-}

-----------------------------------------------------------------------------
-- | A variant of 'atomicModify' in which the computation is strict in the
-- new state.
atomicModify' :: Member (AtomicState s) r
              => (s -> s)
              -> Sem r ()
atomicModify' f = do
  !_ <- atomicState $ \s -> let !s' = f s in (s', ())
  return ()
{-# INLINE atomicModify' #-}

------------------------------------------------------------------------------
-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over an 'IORef'.
runAtomicStateIORef :: Member (Embed IO) r
                    => IORef s
                    -> Sem (AtomicState s ': r) a
                    -> Sem r a
runAtomicStateIORef ref = interpret $ \case
  AtomicState f -> embed $ atomicModifyIORef ref f
  AtomicGet     -> embed $ readIORef ref
{-# INLINE runAtomicStateIORef #-}

------------------------------------------------------------------------------
-- | Run an 'AtomicState' effect by transforming it into atomic operations
-- over a 'TVar'.
runAtomicStateTVar :: Member (Embed IO) r
                   => TVar s
                   -> Sem (AtomicState s ': r) a
                   -> Sem r a
runAtomicStateTVar tvar = interpret $ \case
  AtomicState f -> embed $ atomically $ do
    (s', a) <- f <$> readTVar tvar
    writeTVar tvar s'
    return a
  AtomicGet -> embed $ readTVarIO tvar
{-# INLINE runAtomicStateTVar #-}

------------------------------------------------------------------------------
-- | Transform an 'AtomicState' effect to a 'State' effect, discarding
-- the notion of atomicity.
atomicStateToState :: Member (State s) r
                   => Sem (AtomicState s ': r) a
                   -> Sem r a
atomicStateToState = interpret $ \case
  AtomicState f -> do
    (s', a) <- f <$> get
    put s'
    return a
  AtomicGet -> get
{-# INLINE atomicStateToState #-}
