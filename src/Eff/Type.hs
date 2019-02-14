{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}

{-# OPTIONS_GHC -Wall       #-}

module Eff.Type
  ( module Eff.Type
  , MFunctor (..)
  , MonadTrans (..)
  , Identity ()
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
  fmap f (Freer m) = Freer $ \k -> fmap f $ m k
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


------------------------------------------------------------------------------
-- | Run a natural transformation over `Freer`.
hoistEff :: (f ~> g) -> Freer f ~> Freer g
hoistEff nat (Freer m) = Freer $ \k -> m $ k . nat
{-# INLINE hoistEff #-}


------------------------------------------------------------------------------
-- | Lift a value into 'Freer'. When 'f' is 'Union', this specializes as
-- @Union -- r x -> Eff r x@
liftEff :: f x -> Freer f x
liftEff u = Freer $ \k -> k u
{-# INLINE liftEff #-}


------------------------------------------------------------------------------
-- | A natural transformation from 'f' to 'g' (that is, @forall x. f x -> g x@)
type f ~> g = forall x. f x -> g x
infixr 1 ~>


------------------------------------------------------------------------------
-- | Embed the action of an effect into 'Eff'.
send :: Member eff r => eff a -> Eff r a
send = liftEff . inj
{-# INLINE[3] send #-}


------------------------------------------------------------------------------
-- | Drop out of an 'Eff' stack into the only remaining monadic effect inside
-- it.
runM :: Monad m => Eff '[m] a -> m a
runM = usingFreer extract
{-# INLINE runM #-}


------------------------------------------------------------------------------
-- | Like 'runM' but for pure computations.
run :: Eff '[Identity] a -> a
run = runIdentity . runM
{-# INLINE run #-}


------------------------------------------------------------------------------
-- | @'flip' 'runFreer'@
usingFreer :: Monad m => (forall t. f t -> m t) -> Freer f a -> m a
usingFreer k m = runFreer m k


------------------------------------------------------------------------------
-- | Analogous to MTL's 'lift'.
raise :: Eff r a -> Eff (u ': r) a
raise = hoistEff weaken
{-# INLINE raise #-}

