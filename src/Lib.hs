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
import Data.IORef
import Control.Arrow (second)
import System.IO.Unsafe


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


runTeletype :: forall r a. Member IO r => Eff (State String ': r) a -> Eff r a
runTeletype = interpret bind
  where
    bind :: forall x. State String x -> Eff r x
    bind Get     = send getLine
    bind (Put s) = send $ putStrLn s


main :: IO ()
main = runM (runState "fuck" foom) >>= print


raise :: Eff r a -> Eff (u ': r) a
raise = freeMap weaken


introduce :: Eff (eff ': r) a -> Eff (eff ': u ': r) a
introduce = freeMap (shuffle . weaken)


interpretS
    :: forall eff s r
     . (forall x. s -> eff x -> (s, Eff r x))
    -> s
    -> Eff (eff ': r) ~> Eff r
interpretS f s (Freer m) =
  let !ioref = unsafePerformIO $ newIORef s
   in Freer $ \k -> m $ \u -> do
        case decomp u of
          Left x -> k x
          Right y ->
            let !s' = unsafePerformIO $ readIORef ioref
                (s'', m') = f s' y
             in unsafePerformIO (writeIORef ioref s'') `seq` runFreer m' k


runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r a
runState = interpretS nat
  where
    nat :: s -> State s x -> (s, Eff r x)
    nat s Get      = (s, pure s)
    nat _ (Put s') = (s', pure ())

