{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import Debug.Trace
import Data.OpenUnion
import Data.Functor.Compose
import Data.IORef
import Control.Arrow (second)
import System.IO.Unsafe
import qualified Control.Monad.Trans.State.Strict as S
import Data.Functor.Identity
import Control.Monad.Trans (lift)


newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (forall t. f t -> m t) -> m a
  }

freeMap :: (f ~> g) -> Freer f ~> Freer g
freeMap nat (Freer m) = Freer $ \k -> m $ k . nat
{-# INLINE freeMap #-}

hoistEff :: Union r x -> Eff r x
hoistEff u = Freer $ \k -> k u
{-# INLINE hoistEff #-}

instance Functor (Freer f) where
  fmap f (Freer z) = Freer $ \z' -> fmap f $ z z'
  {-# INLINE fmap #-}

instance Applicative (Freer f) where
  pure a = Freer $ const $ pure a
  {-# INLINE pure #-}
  Freer f <*> Freer a = Freer $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}

instance Monad (Freer f) where
  return = pure
  {-# INLINE return #-}
  Freer ma >>= f = Freer $ \k -> do
    z <- ma k
    runFreer (f z) k
  {-# INLINE (>>=) #-}


type Eff r = Freer (Union r)


send :: Member eff r => eff a -> Eff r a
send t = Freer $ \k -> k $ inj t
{-# INLINE send #-}


runM :: Monad m => Freer (Union '[m]) a -> m a
runM z = runFreer z extract
{-# INLINE runM #-}

run :: Freer (Union '[Identity]) a -> a
run = runIdentity . runM
{-# INLINE run #-}


data State s a where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) r => Eff r s
get = send Get
{-# INLINE get #-}

put :: Member (State s) r => s -> Eff r ()
put = send . Put
{-# INLINE put #-}


foom :: Eff '[State String, IO] String
foom = do
  put "nice!"
  put "nice!"
  put "nice!"
  get @String

type f ~> g = forall x. f x -> g x
infixr 1 ~>

interpret :: (eff ~> Eff r) -> Eff (eff ': r) ~> Eff r
interpret f (Freer m) = Freer $ \k -> m $ \u -> do
  case decomp u of
    Left x -> k x
    Right y -> runFreer (f y) k
{-# INLINE interpret #-}


runTeletype :: forall r a. Member IO r => Eff (State String ': r) a -> Eff r a
runTeletype = interpret bind
  where
    bind :: forall x. State String x -> Eff r x
    bind Get     = send getLine
    bind (Put s) = send $ putStrLn s


interpretState :: s -> Eff (S.State s ': r) ~> Eff r
interpretState s m = flip S.evalStateT s $ runFreer m $ \u ->
  case decomp u of
    Left x  -> S.StateT $ \s -> fmap (, s) $hoistEff x
    Right y -> S.mapStateT (pure . runIdentity) y
{-# INLINE interpretState #-}


-- main :: IO ()
-- main = runM (runState "fuck" foom) >>= print


raise :: Eff r a -> Eff (u ': r) a
raise = freeMap weaken
{-# INLINE raise #-}


introduce :: Eff (eff ': r) a -> Eff (eff ': u ': r) a
introduce = freeMap (shuffle . weaken)
{-# INLINE introduce #-}


interpretS
    :: forall eff s r
     . (forall x. s -> eff x -> (s, Eff r x))
    -> s
    -> Eff (eff ': r) ~> Eff r
interpretS f s = interpretState s . interpret bind . introduce
  where
    bind :: eff x -> Eff (S.State s ': r) x
    bind e = do
      s <- send $ S.get @Identity
      let (s', e') = f s e
      send $ S.put @Identity s'
      raise e'
    {-# INLINE bind #-}
{-# INLINE interpretS #-}



runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r a
runState = interpretS nat
  where
    nat :: s -> State s x -> (s, Eff r x)
    nat s Get      = (s, pure s)
    nat _ (Put s') = (s', pure ())
    {-# INLINE nat #-}
{-# INLINE runState #-}

