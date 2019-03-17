{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE UnicodeSyntax    #-}

module Control.Monad.Discount
  ( module Control.Monad.Discount
  , module Control.Monad.Discount.Effect
  , module Control.Monad.Discount.Lift
  , Member
  , decomp
  , prj
  ) where

import Data.OpenUnion
import Control.Monad.Discount.Effect
import Control.Monad (join)
import Control.Monad.Discount.Lift


type Eff r = F (Union r)


newtype F f a = F
  { runF
        :: ∀ r
         . (a -> r)
        -> (f (F f) r -> r)
        -> r
  }


instance Functor (F f) where
  fmap f (F g) = F (\kp -> g (kp . f))
  {-# INLINE fmap #-}


instance Applicative (F f) where
  pure a = F (\kp _ -> kp a)
  {-# INLINE pure #-}
  F f <*> F g = F (\kp kf -> f (\a -> g (kp . a) kf) kf)
  {-# INLINE (<*>) #-}


instance Monad (F f) where
  return = pure
  {-# INLINE return #-}
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)
  {-# INLINE (>>=) #-}


runEff :: (a -> r) -> (f (F f) r -> r) -> F f a -> r
runEff kp kf e = runF e kp kf
{-# INLINE runEff #-}


liftEff :: Union r (Eff r) a -> Eff r a
liftEff u = F $ \kp kf -> kf $ fmap kp u
{-# INLINE liftEff #-}

raise :: Eff r a -> Eff (e ': r) a
raise = runEff pure $ join . liftEff . hoist raise . weaken
{-# INLINE raise #-}


interpret
    :: (∀ x. e (Eff (e ': r)) (Eff r x) -> Eff r x)
    -> Eff (e ': r) a
    -> Eff r a
interpret f = runEff pure $ \u ->
  case decomp u of
    Left x -> join . liftEff $ hoist (interpret f) x
    Right eff -> f eff
{-# INLINE interpret #-}


interpose
    :: Member e r
    => (∀ x. e (Eff r) x -> Eff r x)
    -> Eff r a
    -> Eff r a
interpose f = runEff pure $ \u ->
  join $ case prj u of
    Just x  -> f x
    Nothing -> liftEff u
{-# INLINE interpose #-}


subsume
    :: (Member e r, Effect e)
    => Eff (e ': r) a
    -> Eff r a
subsume = interpret $ join . send . hoist subsume
{-# INLINE subsume #-}


reinterpret
    :: Effect f
    => (∀ x. f (Eff (g ': r)) x -> Eff (g ': r) x)
    -> Eff (f ': r) a
    -> Eff (g ': r) a
reinterpret f = runEff pure $ \u ->
  join $ case decomp u of
    Left  x -> liftEff $ weaken $ hoist (reinterpret f) x
    Right y -> f $ hoist (reinterpret f) $ y
{-# INLINE reinterpret #-}


-- TODO(sandy): does this have the right semantics for INSIDE MONADS?
translate
    :: ( Effect f
       , Effect g
       )
    => (∀ x. f (Eff (f ': r)) x -> g (Eff (g ': r)) x)
    -> Eff (f ': r) a
    -> Eff (g ': r) a
translate f = runEff pure $ \u ->
  join $ case decomp u of
    Left  x -> liftEff $ weaken $ hoist (translate f) x
    Right y -> send $ f y
{-# INLINE translate #-}


runM :: Monad m => Eff '[Lift m] a -> m a
runM e = runF e pure $ join . unLift . extract
{-# INLINE runM #-}


run :: Eff '[] a -> a
run = runEff id absurdU
{-# INLINE run #-}


send :: Member eff r => eff (Eff r) a -> Eff r a
send = liftEff . inj
{-# INLINE send #-}


sendM :: Member (Lift m) r => m a -> Eff r a
sendM = liftEff . inj . Lift
{-# INLINE sendM #-}

