{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.State
  ( -- * Effect
    State (..)

    -- * Actions
  , get
  , gets
  , put
  , modify
  , modify'

    -- * Interpretations
  , runState
  , evalState
  , execState
  , runLazyState
  , evalLazyState
  , execLazyState
  , runStateIORef
  , stateToIO
  , runStateSTRef
  , stateToST

    -- * Interoperation with MTL
  , hoistStateIntoStateT
  ) where

import           Control.Monad.ST
import qualified Control.Monad.Trans.State as S
import           Data.IORef
import           Data.STRef
import           Data.Tuple (swap)
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Combinators
import           Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | An effect for providing statefulness. Note that unlike mtl's
-- 'Control.Monad.Trans.State.StateT', there is no restriction that the 'State'
-- effect corresponds necessarily to /local/ state. It could could just as well
-- be interrpeted in terms of HTTP requests or database access.
--
-- Interpreters which require statefulness can 'Polysemy.reinterpret'
-- themselves in terms of 'State', and subsequently call 'runState'.
data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()

makeSem ''State


gets :: forall s a r. Member (State s) r => (s -> a) -> Sem r a
gets f = fmap f get
{-# INLINABLE gets #-}


modify :: Member (State s) r => (s -> s) -> Sem r ()
modify f = do
  s <- get
  put $ f s
{-# INLINABLE modify #-}

------------------------------------------------------------------------------
-- | A variant of 'modify' in which the computation is strict in the
-- new state.
modify' :: Member (State s) r => (s -> s) -> Sem r ()
modify' f = do
  s <- get
  put $! f s
{-# INLINABLE modify' #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect with local state.
runState :: s -> Sem (State s ': r) a -> Sem r (s, a)
runState = stateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runState #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect with local state.
--
-- @since 1.0.0.0
evalState :: s -> Sem (State s ': r) a -> Sem r a
evalState s = fmap snd . runState s
{-# INLINE evalState #-}

------------------------------------------------------------------------------
-- | Run a 'State' effect with local state.
--
-- @since 1.2.3.1
execState :: s -> Sem (State s ': r) a -> Sem r s
execState s = fmap fst . runState s
{-# INLINE execState #-}



------------------------------------------------------------------------------
-- | Run a 'State' effect with local state, lazily.
runLazyState :: s -> Sem (State s ': r) a -> Sem r (s, a)
runLazyState = lazilyStateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runLazyState #-}

------------------------------------------------------------------------------
-- | Run a 'State' effect with local state, lazily.
--
-- @since 1.0.0.0
evalLazyState :: s -> Sem (State s ': r) a -> Sem r a
evalLazyState s = fmap snd . runLazyState s
{-# INLINE evalLazyState #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect with local state, lazily.
--
-- @since 1.2.3.1
execLazyState :: s -> Sem (State s ': r) a -> Sem r s
execLazyState s = fmap fst . runLazyState s
{-# INLINE execLazyState #-}



------------------------------------------------------------------------------
-- | Run a 'State' effect by transforming it into operations over an 'IORef'.
--
-- /Note/: This is not safe in a concurrent setting, as 'modify' isn't atomic.
-- If you need operations over the state to be atomic,
-- use 'Polysemy.AtomicState.runAtomicStateIORef' or
-- 'Polysemy.AtomicState.runAtomicStateTVar' instead.
--
-- @since 1.0.0.0
runStateIORef
    :: forall s r a
     . Member (Embed IO) r
    => IORef s
    -> Sem (State s ': r) a
    -> Sem r a
runStateIORef ref = interpret $ \case
  Get   -> embed $ readIORef ref
  Put s -> embed $ writeIORef ref s
{-# INLINE runStateIORef #-}

--------------------------------------------------------------------
-- | Run an 'State' effect in terms of operations
-- in 'IO'.
--
-- Internally, this simply creates a new 'IORef', passes it to
-- 'runStateIORef', and then returns the result and the final value
-- of the 'IORef'.
--
-- /Note/: This is not safe in a concurrent setting, as 'modify' isn't atomic.
-- If you need operations over the state to be atomic,
-- use 'Polysemy.AtomicState.atomicStateToIO' instead.
--
-- /Beware/: As this uses an 'IORef' internally,
-- all other effects will have local
-- state semantics in regards to 'State' effects
-- interpreted this way.
-- For example, 'Polysemy.Error.throw' and 'Polysemy.Error.catch' will
-- never revert 'put's, even if 'Polysemy.Error.runError' is used
-- after 'stateToIO'.
--
-- @since 1.2.0.0
stateToIO
    :: forall s r a
     . Member (Embed IO) r
    => s
    -> Sem (State s ': r) a
    -> Sem r (s, a)
stateToIO s sem = do
  ref <- embed $ newIORef s
  res <- runStateIORef ref sem
  end <- embed $ readIORef ref
  return (end, res)
{-# INLINE stateToIO #-}

------------------------------------------------------------------------------
-- | Run a 'State' effect by transforming it into operations over an 'STRef'.
--
-- @since 1.3.0.0
runStateSTRef
    :: forall s st r a
     . Member (Embed (ST st)) r
    => STRef st s
    -> Sem (State s ': r) a
    -> Sem r a
runStateSTRef ref = interpret $ \case
  Get   -> embed $ readSTRef ref
  Put s -> embed $ writeSTRef ref s
{-# INLINE runStateSTRef #-}

--------------------------------------------------------------------
-- | Run an 'State' effect in terms of operations
-- in 'ST'.
--
-- Internally, this simply creates a new 'STRef', passes it to
-- 'runStateSTRef', and then returns the result and the final value
-- of the 'STRef'.
--
-- /Beware/: As this uses an 'STRef' internally,
-- all other effects will have local
-- state semantics in regards to 'State' effects
-- interpreted this way.
-- For example, 'Polysemy.Error.throw' and 'Polysemy.Error.catch' will
-- never revert 'put's, even if 'Polysemy.Error.runError' is used
-- after 'stateToST'.
--
-- When not using the plugin, one must introduce the existential @st@ type to
-- 'stateToST', so that the resulting type after 'runM' can be resolved into
-- @forall st. ST st (s, a)@ for use with 'runST'. Doing so requires
-- @-XScopedTypeVariables@.
--
-- @
-- stResult :: forall s a. (s, a)
-- stResult = runST ( (runM $ stateToST \@_ \@st undefined $ pure undefined) :: forall st. ST st (s, a) )
-- @
--
-- @since 1.3.0.0
stateToST
    :: forall s st r a
     . Member (Embed (ST st)) r
    => s
    -> Sem (State s ': r) a
    -> Sem r (s, a)
stateToST s sem = do
  ref <- embed @(ST st) $ newSTRef s
  res <- runStateSTRef ref sem
  end <- embed $ readSTRef ref
  return (end, res)
{-# INLINE stateToST #-}

------------------------------------------------------------------------------
-- | Hoist a 'State' effect into a 'S.StateT' monad transformer. This can be
-- useful when writing interpreters that need to interop with MTL.
--
-- @since 0.1.3.0
hoistStateIntoStateT
    :: Sem (State s ': r) a
    -> S.StateT s (Sem r) a
hoistStateIntoStateT (Sem m) = m $ \u ->
  case decomp u of
    Left x -> S.StateT $ \s ->
      liftSem . fmap swap
              . weave (s, ())
                      (\(s', m') -> fmap swap
                                  $ S.runStateT m' s')
                      (Just . snd)
              $ hoist hoistStateIntoStateT x
    Right (Weaving Get z _ y _)     -> fmap (y . (<$ z)) $ S.get
    Right (Weaving (Put s) z _ y _) -> fmap (y . (<$ z)) $ S.put s
{-# INLINE hoistStateIntoStateT #-}


{-# RULES "runState/reinterpret"
   forall s e (f :: forall m x. e m x -> Sem (State s ': r) x).
     runState s (reinterpret f e) = stateful (\x s' -> runState s' $ f x) s e
     #-}

{-# RULES "runLazyState/reinterpret"
   forall s e (f :: forall m x. e m x -> Sem (State s ': r) x).
     runLazyState s (reinterpret f e) = lazilyStateful (\x s' -> runLazyState s' $ f x) s e
     #-}

