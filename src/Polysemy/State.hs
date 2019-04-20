{-# LANGUAGE TemplateHaskell #-}

module Polysemy.State
  ( -- * Effect
    State (..)

    -- * Actions
  , get
  , gets
  , put
  , modify

    -- * Interpretations
  , runState
  , runLazyState
  , runStateInIORef
  ) where

import Data.IORef
import Polysemy
import Polysemy.Internal.Combinators


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


gets :: Member (State s) r => (s -> a) -> Sem r a
gets f = fmap f get
{-# INLINABLE gets #-}


modify :: Member (State s) r => (s -> s) -> Sem r ()
modify f = do
  s <- get
  put $ f s
{-# INLINABLE modify #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect with local state.
runState :: Typeable s => s -> Sem (State s ': r) a -> Sem r (s, a)
runState = stateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runState #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect with local state, lazily.
runLazyState :: Typeable s => s -> Sem (State s ': r) a -> Sem r (s, a)
runLazyState = lazilyStateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runLazyState #-}


------------------------------------------------------------------------------
-- | Run a 'State' effect by transforming it into operations over an 'IORef'.
--
-- @since 0.1.2.0
runStateInIORef
    :: forall s r a
     . Member (Lift IO) r
    => IORef s
    -> Sem (State s ': r) a
    -> Sem r a
runStateInIORef ref = interpret $ \case
  Get   -> sendM $ readIORef ref
  Put s -> sendM $ writeIORef ref s
{-# INLINE runStateInIORef #-}


{-# RULES "runState/reinterpret"
   forall s e (f :: forall m x. e m x -> Sem (State s ': r) x).
     runState s (reinterpret f e) = stateful (\x s' -> runState s' $ f x) s e
     #-}

{-# RULES "runLazyState/reinterpret"
   forall s e (f :: forall m x. e m x -> Sem (State s ': r) x).
     runLazyState s (reinterpret f e) = lazilyStateful (\x s' -> runLazyState s' $ f x) s e
     #-}

