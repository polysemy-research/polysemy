{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE DeriveAnyClass   #-}
{-# LANGUAGE DeriveFunctor    #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE PolyKinds        #-}
{-# LANGUAGE TypeOperators    #-}

module Definitive.State
  ( State (..)
  , get
  , put
  , modify
  , runState
  ) where

import qualified Control.Monad.Trans.State.Strict as S
import           Definitive
import           Definitive.Effect


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)


get :: Member (State s) r => Def r s
get = send $ Get id
{-# INLINE get #-}


put :: Member (State s) r => s -> Def r ()
put s = send $ Put s ()
{-# INLINE put #-}


modify :: Member (State s) r => (s -> s) -> Def r ()
modify f = do
  s <- get
  put $ f s
{-# INLINE modify #-}


runState :: s -> Def (State s ': r) a -> Def r (s, a)
runState = stateful $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE[3] runState #-}

{-# RULES "runState/reinterpret"
   forall s e (f :: forall x. e (Def (e ': r)) x -> Def (State s ': r) x).
     runState s (reinterpret f e) = runRelayS (\x s' -> runState s' $ f x) s e
     #-}

