{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module Polysemy.State
  ( State (..)
  , get
  , put
  , modify
  , runState
  ) where

import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy
import           Polysemy.Effect


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)


get :: Member (State s) r => Poly r s
get = send $ Get id
{-# INLINE get #-}


put :: Member (State s) r => s -> Poly r ()
put s = send $ Put s ()
{-# INLINE put #-}


modify :: Member (State s) r => (s -> s) -> Poly r ()
modify f = do
  s <- get
  put $ f s
{-# INLINE modify #-}


runState :: s -> Poly (State s ': r) a -> Poly r (s, a)
runState = stateful $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE[3] runState #-}

{-# RULES "runState/reinterpret"
   forall s e (f :: forall x. e (Poly (e ': r)) x -> Poly (State s ': r) x).
     runState s (reinterpret f e) = runRelayS (\x s' -> runState s' $ f x) s e
     #-}

