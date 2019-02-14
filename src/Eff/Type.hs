{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

{-# OPTIONS_GHC -Wall               #-}

module Eff.Type
  ( module Eff.Type
  , MFunctor (..)
  , MonadTrans (..)
  ) where

import Control.Monad.Morph
import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity
import Data.OpenUnion


type Eff r = Freer (Union r)

newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (f ~> m) -> m a
  }

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

instance MonadTrans Freer where
  lift = liftEff

instance MFunctor Freer where
  hoist = hoistEff

hoistEff :: (f ~> g) -> Freer f ~> Freer g
hoistEff nat (Freer m) = Freer $ \k -> m $ k . nat
{-# INLINE hoistEff #-}


liftEff :: f x -> Freer f x
liftEff u = Freer $ \k -> k u
{-# INLINE liftEff #-}


type f ~> g = forall x. f x -> g x
infixr 1 ~>


send :: Member eff r => eff a -> Eff r a
send t = Freer $ \k -> k $ inj t
{-# INLINE send #-}


runM :: Monad m => Freer (Union '[m]) a -> m a
runM = runIt extract
{-# INLINE runM #-}


run :: Freer (Union '[Identity]) a -> a
run = runIdentity . runM
{-# INLINE run #-}


runIt :: Monad m => (forall t. f t -> m t) -> Freer f a -> m a
runIt k m = runFreer m k

------------------------------------------------------------------------------
-- | Analogous to MTL's 'lift'.
raise :: Eff r a -> Eff (u ': r) a
raise = hoistEff weaken
{-# INLINE raise #-}

