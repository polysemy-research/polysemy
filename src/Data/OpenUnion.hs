{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -Wall              #-}

module Data.OpenUnion where

import Control.Monad.Discount.Effect
import Data.Typeable

data Dict c where Dict :: c => Dict c


data Nat = Z | S Nat
  deriving Typeable

data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: Typeable n => SNat n -> SNat ('S n)
  deriving Typeable


type family IndexOf (ts :: [k]) (n :: Nat) :: k where
  IndexOf (k ': ks) 'Z = k
  IndexOf (k ': ks) ('S n) = IndexOf ks n


type family Found (ts :: [k]) (t :: k) :: Nat where
  Found (t ': ts) t = 'Z
  Found (u ': ts) t = 'S (Found ts t)


class Typeable (Found r t) => Find (r :: [k]) (t :: k) where
  finder :: SNat (Found r t)

instance {-# OVERLAPPING #-} Find (t ': z) t where
  finder = SZ
  {-# INLINE finder #-}

instance ( Find z t
         , Found (_1 ': z) t ~ 'S (Found z t)
         ) => Find (_1 ': z) t where
  finder = SS $ finder @_ @z @t
  {-# INLINE finder #-}




data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union
      :: Effect (IndexOf r n)
      => SNat n
      -> IndexOf r n m a
      -> Union r m a


decomp :: Union (e ': r) m a -> Either (Union r m a) (e m a)
decomp (Union p a) =
  case p of
    SZ   -> Right a
    SS n -> Left $ Union n a
{-# INLINE decomp #-}


extract :: Union '[e] m a -> e m a
extract (Union SZ a) = a
extract _ = error "impossible"
{-# INLINE extract #-}


weaken :: Union r m a -> Union (e ': r) m a
weaken (Union n a) =
  case induceTypeable n of
    Dict -> Union (SS n) a
{-# INLINE weaken #-}


inj
    :: forall r e a m
     . ( Functor m
       , Find r e
       , Effect e
       , e ~ IndexOf r (Found r e)
       )
    => e m a
    -> Union r m a
inj e = Union (finder @_ @r @e) e
{-# INLINE inj #-}


induceTypeable :: SNat n -> Dict (Typeable n)
induceTypeable SZ = Dict
induceTypeable (SS _) = Dict
{-# INLINE induceTypeable #-}


type Member e r = (Find r e, e ~ IndexOf r (Found r e), Effect e)



prj
    :: forall r e a m
     . ( Find r e
       , e ~ IndexOf r (Found r e)
       )
    => Union r m a
    -> Maybe (e m a)
prj (Union (s :: SNat n) a) =
  case induceTypeable s of
    Dict ->
      case eqT @n @(Found r e) of
        Just Refl -> Just a
        Nothing -> Nothing
{-# INLINE prj #-}


instance Effect (Union r) where
  weave s f (Union w e) = Union w $ weave s f e
  {-# INLINE weave #-}


instance (Functor m) => Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap' f t
    where
      fmap' :: (Functor m, Effect f) => (a -> b) -> f m a -> f m b
      fmap' = fmap
      {-# INLINE fmap' #-}
  {-# INLINE fmap #-}

