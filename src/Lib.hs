{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

module Lib where

import Debug.Trace
import Data.OpenUnion
import Data.Functor.Compose
import Control.Monad.ST
import Data.STRef
import Control.Arrow (second)
import Unsafe.Coerce

newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (forall t. f t -> m t) -> m a
  }

freeMap :: (f ~> g) -> Freer f ~> Freer g
freeMap nat (Freer m) = Freer $ \k -> m $ k . nat

instance Functor (Freer f) where
  fmap f (Freer z) = Freer $ \z' -> fmap f $ z z'

instance Applicative (Freer f) where
  pure a = Freer $ const $ pure a
  Freer f <*> Freer a = Freer $ \k -> f k <*> a k

instance Monad (Freer f) where
  return = pure
  Freer ma >>= f = Freer $ \k -> do
    z <- ma k
    runFreer (f z) k


type Eff r = Freer (Union r)


send :: Member eff r => eff a -> Eff r a
send t = Freer $ \k -> k $ inj t


runM :: Monad m => Freer (Union '[m]) a -> m a
runM z = runFreer z extract


data State s a where
  Get :: State s s
  Put :: s -> State s ()

get :: Member (State s) r => Eff r s
get = send Get

put :: Member (State s) r => s -> Eff r ()
put = send . Put


foom :: Eff '[State String, IO] String
foom = do
  put "nice!"
  get @String

type f ~> g = forall x. f x -> g x
infixr 1 ~>

interpret :: (eff ~> Eff r) -> Eff (eff ': r) ~> Eff r
interpret f (Freer m) = Freer $ \k -> m $ \u -> do
  case decomp u of
    Left x -> k x
    Right y -> runFreer (f y) k


runTeletype :: forall r a. Member IO r => Eff (State String ': r) a -> Eff r a
runTeletype = interpret bind
  where
    bind :: forall x. State String x -> Eff r x
    bind Get     = send getLine
    bind (Put s) = send $ putStrLn s

main :: IO ()
main = runM (runState "ok" foom) >>= print

interpretS
    :: ((,) s `Compose` eff ~> (,) s `Compose` Eff r)
    -> s
    -> Eff (eff ': r) ~> Eff r
interpretS f s mm =
  let (Freer m) = freeMap (\z -> Compose (unsafeCoerce "hi", z)) mm
   in freeMap (snd . getCompose) $ Freer $ \k -> m $ \(Compose (s', u)) ->
        case decomp $ u of
          Left x -> k $ Compose (s', x)
          Right y ->
            let (s'', e) = getCompose $ f $ Compose (s', y)
             in runFreer (freeMap (\z -> Compose (s'', z)) e) k


runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r a
runState = interpretS nat
  where
    nat :: (,) s `Compose` State s ~> (,) s `Compose` Eff r
    nat (Compose (s, Get))    = Compose (s, pure s)
    nat (Compose (_, Put s')) = Compose (s', pure ())


