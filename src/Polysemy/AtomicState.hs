{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Description: The 'AtomicState' effect
module Polysemy.AtomicState
  ( -- * Effect
    AtomicState (..)

    -- * Actions
  , atomicState
  , atomicState'
  , atomicGet
  , atomicGets
  , atomicPut
  , atomicModify
  , atomicModify'

    -- * Interpretations
  , runAtomicStateIORef
  , runAtomicStateTVar
  , atomicStateToIO
  , atomicStateToState
  , runAtomicStateViaState
  , evalAtomicStateViaState
  , execAtomicStateViaState
  ) where


import Control.Concurrent.STM

import Polysemy
import Polysemy.State

import Data.IORef

------------------------------------------------------------------------------
-- | A variant of 'State' that supports atomic operations.
--
-- @since 1.1.0.0
data AtomicState s m a where
  -- | Run a state action.
  AtomicState :: (s -> (s, a)) -> AtomicState s m a
  -- | Get the state.
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

------------------------------------------------------------------------------
-- | @since 1.2.2.0
atomicGets :: forall s s' r
            . Member (AtomicState s) r
           => (s -> s')
           -> Sem r s'
atomicGets = (<$> atomicGet)
{-# INLINE atomicGets #-}

-----------------------------------------------------------------------------
-- | A variant of 'atomicState' in which the computation is strict in the new
-- state and return value.
atomicState' :: forall s a r
              . Member (AtomicState s) r
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

-----------------------------------------------------------------------------
-- | Replace the state with the given value.
atomicPut :: Member (AtomicState s) r
          => s
          -> Sem r ()
atomicPut s = do
  !_ <- atomicState $ \_ -> (s, ()) -- strict put with atomicModifyIORef
  return ()
{-# INLINE atomicPut #-}

-----------------------------------------------------------------------------
-- | Modify the state lazily.
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
runAtomicStateIORef :: forall s r a
                     . Member (Embed IO) r
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

--------------------------------------------------------------------
-- | Run an 'AtomicState' effect in terms of atomic operations
-- in 'IO'.
--
-- Internally, this simply creates a new 'IORef', passes it to
-- 'runAtomicStateIORef', and then returns the result and the final value
-- of the 'IORef'.
--
-- /Note/: As this uses an 'IORef' internally,
-- all other effects will have local
-- state semantics in regards to 'AtomicState' effects
-- interpreted this way.
-- For example, 'Polysemy.Error.throw' and 'Polysemy.Error.catch' will
-- never revert 'atomicModify's, even if 'Polysemy.Error.runError' is used
-- after 'atomicStateToIO'.
--
-- @since 1.2.0.0
atomicStateToIO :: forall s r a
                 . Member (Embed IO) r
                => s
                -> Sem (AtomicState s ': r) a
                -> Sem r (s, a)
atomicStateToIO s sem = do
  ref <- embed $ newIORef s
  res <- runAtomicStateIORef ref sem
  end <- embed $ readIORef ref
  return (end, res)
{-# INLINE atomicStateToIO #-}

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

------------------------------------------------------------------------------
-- | Run an 'AtomicState' with local state semantics, discarding
-- the notion of atomicity, by transforming it into 'State' and running it
-- with the provided initial state.
--
--
-- @since v1.7.0.0
runAtomicStateViaState :: s
                       -> Sem (AtomicState s ': r) a
                       -> Sem r (s, a)
runAtomicStateViaState s =
  runState s . atomicStateToState . raiseUnder
{-# INLINE runAtomicStateViaState #-}

------------------------------------------------------------------------------
-- | Evaluate an 'AtomicState' with local state semantics, discarding
-- the notion of atomicity, by transforming it into 'State' and running it
-- with the provided initial state.
--
-- @since v1.7.0.0
evalAtomicStateViaState :: s
                        -> Sem (AtomicState s ': r) a
                        -> Sem r a
evalAtomicStateViaState s =
  evalState s . atomicStateToState . raiseUnder
{-# INLINE evalAtomicStateViaState #-}

------------------------------------------------------------------------------
-- | Execute an 'AtomicState' with local state semantics, discarding
-- the notion of atomicity, by transforming it into 'State' and running it
-- with the provided initial state.
--
-- @since v1.7.0.0
execAtomicStateViaState :: s
                        -> Sem (AtomicState s ': r) a
                        -> Sem r s
execAtomicStateViaState s =
  execState s . atomicStateToState . raiseUnder
{-# INLINE execAtomicStateViaState #-}
