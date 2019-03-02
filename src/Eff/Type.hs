{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE RankNTypes     #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE ViewPatterns   #-}

{-# OPTIONS_GHC -Wall       #-}

module Eff.Type
  ( module Eff.Type
  , MonadTrans (..)
  , Identity ()
  , type (~>)
  , HFunctor (..)
  ) where

import Control.Monad.Trans (MonadTrans (..))
import Data.Functor.Identity
import Data.OpenUnion.Internal


newtype Lift m (n :: * -> *) a = Lift { unLift :: m a }

type Eff (r :: [(* -> *) -> * -> *]) = Freer (Union r)

newtype Freer f a = Freer
  { runFreer :: forall m. Monad m => (f m ~> m) -> m a
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


------------------------------------------------------------------------------
-- | Run a natural transformation over `Freer`.
hoistEff :: (forall m. f m ~> g m) -> Freer f ~> Freer g
hoistEff nat (Freer m) = Freer $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistEff #-}


------------------------------------------------------------------------------
-- | Lift a value into 'Freer'. When 'f' is 'Union', this specializes as
-- @Union -- r x -> Eff r x@
liftEff :: (forall m. f m x) -> Freer f x
liftEff u = Freer $ \k -> k u
{-# INLINE liftEff #-}



------------------------------------------------------------------------------
-- | Embed the action of an effect into 'Eff'.
send :: Member eff r => (forall m. eff m a) -> Eff r a
send s = liftEff $ inj s
{-# INLINE[3] send #-}


------------------------------------------------------------------------------
-- | Drop out of an 'Eff' stack into the only remaining monadic effect inside
-- it.
runM :: Monad m => Eff '[Lift m] a -> m a
runM = usingFreer $ \(extract -> FreeYo e n f) -> _
{-# INLINE runM #-}


--------------------------------------------------------------------------------
---- | Like 'runM' but for pure computations.
--run :: Eff '[Identity] a -> a
--run = runIdentity . runM
--{-# INLINE run #-}


------------------------------------------------------------------------------
-- | @'flip' 'runFreer'@
usingFreer :: Monad m => (f m ~> m) -> Freer f a -> m a
usingFreer k m = runFreer m k


------------------------------------------------------------------------------
-- | Analogous to MTL's 'lift'.
raise :: Eff r a -> Eff (u ': r) a
raise = hoistEff weaken
{-# INLINE raise #-}

