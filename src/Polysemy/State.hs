{-# LANGUAGE TemplateHaskell #-}

module Polysemy.State
  ( State (..)
  , get
  , gets
  , put
  , modify
  , runState
  , runLazyState
  ) where

import Polysemy


data State s m a where
  Get :: State s m s
  Put :: s -> State s m ()

makeSemantic ''State


gets :: Member (State s) r => (s -> a) -> Semantic r a
gets f = fmap f get
{-# INLINE gets #-}


modify :: Member (State s) r => (s -> s) -> Semantic r ()
modify f = do
  s <- get
  put $ f s
{-# INLINE modify #-}


runState :: s -> Semantic (State s ': r) a -> Semantic r (s, a)
runState = stateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runState #-}


runLazyState :: s -> Semantic (State s ': r) a -> Semantic r (s, a)
runLazyState = lazilyStateful $ \case
  Get   -> \s -> pure (s, s)
  Put s -> const $ pure (s, ())
{-# INLINE[3] runLazyState #-}


{-# RULES "runState/reinterpret"
   forall s e (f :: forall m x. e m x -> Semantic (State s ': r) x).
     runState s (reinterpret f e) = stateful (\x s' -> runState s' $ f x) s e
     #-}

{-# RULES "runLazyState/reinterpret"
   forall s e (f :: forall m x. e m x -> Semantic (State s ': r) x).
     runLazyState s (reinterpret f e) = lazilyStateful (\x s' -> runLazyState s' $ f x) s e
     #-}

