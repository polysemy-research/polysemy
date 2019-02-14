{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wall              #-}

module Lib where

import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Morph
import           Control.Monad.Trans (MonadTrans (..))
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Functor.Identity
import           Data.OpenUnion
import           Data.OpenUnion.Internal


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

unsafeSend :: Word -> eff a -> Eff r a
unsafeSend w t = Freer $ \k -> k $ unsafeInj w t
{-# INLINE unsafeSend #-}


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


-- main :: IO ()
-- main = runM (runState "fuck" foom) >>= print


raise :: Eff r a -> Eff (u ': r) a
raise = freeMap weaken
{-# INLINE raise #-}


introduce :: Eff (eff ': r) a -> Eff (eff ': u ': r) a
introduce = freeMap intro
{-# INLINE introduce #-}


interpretS
    :: (eff ~> S.StateT s (Eff r))
    -> s
    -> Eff (eff ': r) ~> Eff r
interpretS f s = transform (flip S.evalStateT s) f
{-# INLINE interpretS #-}


transform
    :: ( MonadTrans t
       , MFunctor t
       , forall m. Monad m => Monad (t m)
       )
    => (forall m. Monad m => t m a -> m b)
    -> (eff ~> t (Eff r))
    -> Eff (eff ': r) a -> Eff r b
transform lower f (Freer m) = Freer $ \k -> lower $ m $ \u ->
  case decomp u of
    Left  x -> lift $ k x
    Right y -> hoist (\z -> runFreer z k) $ f y
{-# INLINE transform #-}


runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r a
runState = interpretS $ \case
  Get    -> S.get
  Put s' -> S.put s'

newtype Error e r where
  Error :: e -> Error e r

throwError :: Member (Error e) r => e -> Eff r a
throwError = send . Error
{-# INLINE throwError #-}

runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError = transform E.runExceptT $ \(Error e) -> E.throwE e

