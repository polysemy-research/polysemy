{-# LANGUAGE DeriveAnyClass     #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TemplateHaskell    #-}

module Polysemy.State
  ( State (..)
  , get
  , gets
  , put
  , modify
  , runState
  ) where

import Polysemy
import Polysemy.Effect.New


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)

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
  Get k   -> \s -> pure (s, k s)
  Put s k -> const $ pure (s, k)
{-# INLINE[3] runState #-}

{-# RULES "runState/reinterpret"
   forall s e (f :: forall x. e (Semantic (e ': r)) x -> Semantic (State s ': r) x).
     runState s (reinterpret f e) = stateful (\x s' -> runState s' $ f x) s e
     #-}

