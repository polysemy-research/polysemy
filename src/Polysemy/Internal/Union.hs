{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StrictData            #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Polysemy.Internal.Union
  ( Union (..)
  , Yo (..)
  , liftYo
  , Member
  -- * Building Unions
  , inj
  , weaken
  -- * Using Unions
  , decomp
  , prj
  , extract
  , absurdU
  , decompCoerce
  -- * Witnesses
  , SNat (..)
  , Nat (..)
  ) where

import Data.Functor.Compose
import Data.Functor.Identity
import Data.Typeable
import Polysemy.Internal.Effect

#ifdef ERROR_MESSAGES
import Polysemy.Union.TypeErrors
#endif


------------------------------------------------------------------------------
-- | An extensible, type-safe union. The @r@ type parameter is a type-level
-- list of effects, any one of which may be held within the 'Union'.
data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union
      :: SNat n
         -- ^ A proof that the effect is actually in @r@.
      -> Yo (IndexOf r n) m a
         -- ^ The effect to wrap. The functions 'prj' and 'decomp' can help
         -- retrieve this value later.
      -> Union r m a



data Yo e m a where
  Yo :: (Functor f)
     => e m a
     -> f ()
     -> (forall x. f (m x) -> n (f x))
     -> (f a -> b)
     -> Yo e n b

liftYo :: Functor m => e m a -> Yo e m a
liftYo e = Yo e (Identity ()) (fmap Identity . runIdentity) runIdentity

instance Functor (Yo e m) where
  fmap f (Yo e s d f') = Yo e s d (f . f')

instance Effect (Yo e) where
  weave s' d (Yo e s nt f) =
    Yo e (Compose $ s <$ s')
         (fmap Compose . d . fmap nt . getCompose)
         (fmap f . getCompose)

  hoist = defaultHoist


instance (Functor m) => Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap' f t
    where
      -- This is necessary to delay the interaction between the type family
      -- 'IndexOf' and the quantified superclass constraint on 'Effect'.
      fmap' :: (Functor m, Effect f) => (a -> b) -> f m a -> f m b
      fmap' = fmap
      {-# INLINE fmap' #-}
  {-# INLINE fmap #-}


instance Effect (Union r) where
  weave s f (Union w e) = Union w $ weave s f e
  {-# INLINE weave #-}

  hoist f (Union w e) = Union w $ hoist f e
  {-# INLINE hoist #-}



-- TODO(sandy): This type error gets in the way of real type errors, eg if you
-- put a $ in the wrong place
type Member e r =
  ( Find r e
  , e ~ IndexOf r (Found r e)
#ifdef ERROR_MESSAGES
  , Break (AmbiguousSend r e) (IndexOf r (Found r e))
#endif
  )


data Dict c where Dict :: c => Dict c


induceTypeable :: SNat n -> Dict (Typeable n)
induceTypeable SZ = Dict
induceTypeable (SS _) = Dict
{-# INLINE induceTypeable #-}


------------------------------------------------------------------------------
-- | The kind of type-level natural numbers.
data Nat = Z | S Nat
  deriving Typeable


------------------------------------------------------------------------------
-- | A singleton for 'Nat'.
data SNat :: Nat -> * where
  SZ :: SNat 'Z
  SS :: Typeable n => SNat n -> SNat ('S n)
  deriving Typeable


type family IndexOf (ts :: [k]) (n :: Nat) :: k where
  IndexOf (k ': ks) 'Z = k
  IndexOf (k ': ks) ('S n) = IndexOf ks n


type family Found (ts :: [k]) (t :: k) :: Nat where
#ifdef ERROR_MESSAGES
  Found '[]       t = UnhandledEffect 'S t
#endif
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


------------------------------------------------------------------------------
-- | Decompose a 'Union'. Either this union contains an effect @e@---the head
-- of the @r@ list---or it doesn't.
decomp :: Union (e ': r) m a -> Either (Union r m a) (Yo e m a)
decomp (Union p a) =
  case p of
    SZ   -> Right a
    SS n -> Left $ Union n a
{-# INLINE decomp #-}


------------------------------------------------------------------------------
-- | Retrieve the last effect in a 'Union'.
extract :: Union '[e] m a -> Yo e m a
extract (Union SZ a) = a
extract _ = error "impossible"
{-# INLINE extract #-}


------------------------------------------------------------------------------
-- | An empty union contains nothing, so this function is uncallable.
absurdU :: Union '[] m a -> b
absurdU = absurdU


------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a new sort of effect.
weaken :: Union r m a -> Union (e ': r) m a
weaken (Union n a) =
  case induceTypeable n of
    Dict -> Union (SS n) a
{-# INLINE weaken #-}


------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it.
inj :: forall r e a m. (Functor m , Member e r) => e m a -> Union r m a
inj e = Union (finder @_ @r @e) $ liftYo e
{-# INLINE inj #-}


------------------------------------------------------------------------------
-- | Attempt to take an @e@ effect out of a 'Union'.
prj :: forall e r a m
     . ( Member e r
       )
    => Union r m a
    -> Maybe (Yo e m a)
prj (Union (s :: SNat n) a) =
  case induceTypeable s of
    Dict ->
      case eqT @n @(Found r e) of
        Just Refl -> Just a
        Nothing -> Nothing
{-# INLINE prj #-}


------------------------------------------------------------------------------
-- | Like 'decomp', but allows for a more efficient
-- 'Polysemy.Interpretation.reinterpret' function.
decompCoerce
    :: Union (e ': r) m a
    -> Either (Union (f ': r) m a) (Yo e m a)
decompCoerce (Union p a) =
  case p of
    SZ -> Right a
    SS n -> Left (Union (SS n) a)
{-# INLINE decompCoerce #-}


