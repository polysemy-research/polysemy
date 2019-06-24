{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE StrictData              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Union
  ( Union (..)
  , Yo (..)
  , Member
  , weave
  , hoist
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
  , LastMember (..)
  ) where

import Data.Bifunctor
import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Type.Equality
import Polysemy.Internal.Kind

#ifndef NO_ERROR_MESSAGES
import Polysemy.Internal.CustomErrors
#endif


------------------------------------------------------------------------------
-- | An extensible, type-safe union. The @r@ type parameter is a type-level
-- list of effects, any one of which may be held within the 'Union'.
data Union (r :: EffectRow) (m :: Type -> Type) a where
  Union
      :: -- A proof that the effect is actually in @r@.
         SNat n
         -- The effect to wrap. The functions 'prj' and 'decomp' can help
         -- retrieve this value later.
      -> Yo (IndexOf r n) m a
      -> Union r m a

instance Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap f t
  {-# INLINE fmap #-}


data Yo e m a where
  Yo :: Functor f
     => e m a
     -> f ()
     -> (forall x. f (m x) -> n (f x))
     -> (f a -> b)
     -> (forall x. f x -> Maybe x)
     -> Yo e n b

instance Functor (Yo e m) where
  fmap f (Yo e s d f' v) = Yo e s d (f . f') v
  {-# INLINE fmap #-}



weave
    :: (Functor s, Functor m, Functor n)
    => s ()
    -> (∀ x. s (m x) -> n (s x))
    -> (∀ x. s x -> Maybe x)
    -> Union r m a
    -> Union r n (s a)
weave s' d v' (Union w (Yo e s nt f v)) = Union w $
    Yo e (Compose $ s <$ s')
         (fmap Compose . d . fmap nt . getCompose)
         (fmap f . getCompose)
         (v <=< v' . getCompose)
{-# INLINE weave #-}


hoist
    :: ( Functor m
       , Functor n
       )
    => (∀ x. m x -> n x)
    -> Union r m a
    -> Union r n a
hoist f' (Union w (Yo e s nt f v)) = Union w $ Yo e s (f' . nt) f v
{-# INLINE hoist #-}


------------------------------------------------------------------------------
-- | A proof that the effect @e@ is available somewhere inside of the effect
-- stack @r@.
type Member e r = Member' e r

type Member' e r =
  ( MemberNoError e r
#ifndef NO_ERROR_MESSAGES
  , Break (AmbiguousSend r e) (IndexOf r (Found r e))
#endif
  )

type MemberNoError e r =
  ( Find r e
  , e ~ IndexOf r (Found r e)
  )


------------------------------------------------------------------------------
-- | The kind of type-level natural numbers.
data Nat = Z | S Nat


------------------------------------------------------------------------------
-- | A singleton for 'Nat'.
data SNat :: Nat -> Type where
  SZ :: SNat 'Z
  SS :: SNat n -> SNat ('S n)

instance TestEquality SNat where
  testEquality SZ     SZ     = Just Refl
  testEquality (SS _) SZ     = Nothing
  testEquality SZ     (SS _) = Nothing
  testEquality (SS n) (SS m) =
    case testEquality n m of
      Nothing -> Nothing
      Just Refl -> Just Refl
  {-# INLINE testEquality #-}


type family IndexOf (ts :: [k]) (n :: Nat) :: k where
  IndexOf (k ': ks) 'Z = k
  IndexOf (k ': ks) ('S n) = IndexOf ks n


type family Found (ts :: [k]) (t :: k) :: Nat where
#ifndef NO_ERROR_MESSAGES
  Found '[]       t = UnhandledEffect 'S t
#endif
  Found (t ': ts) t = 'Z
  Found (u ': ts) t = 'S (Found ts t)


class Find (r :: [k]) (t :: k) where
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
weaken :: forall e r m a. Union r m a -> Union (e ': r) m a
weaken (Union n a) = Union (SS n) a
{-# INLINE weaken #-}



------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it.
inj :: forall r e a m. (Functor m , Member e r) => e m a -> Union r m a
inj e = Union (finder @_ @r @e) $
  Yo e (Identity ())
       (fmap Identity . runIdentity)
       runIdentity
       (Just . runIdentity)
{-# INLINE inj #-}


------------------------------------------------------------------------------
-- | Attempt to take an @e@ effect out of a 'Union'.
prj :: forall e r a m
     . ( Member e r
       )
    => Union r m a
    -> Maybe (Yo e m a)
prj (Union sn a) =
  let sm = finder @_ @r @e
   in case testEquality sn sm of
        Nothing -> Nothing
        Just Refl -> Just a
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


------------------------------------------------------------------------------
-- | A proof that @end@ is the last effect in the row.
--
-- TODO(sandy): @since
class MemberNoError end r => LastMember end r | r -> end where
  decompLast
      :: Union r m a
      -> Either (Union r m a) (Union '[end] m a)

instance {-# OVERLAPPABLE #-} (LastMember end r, MemberNoError end (eff ': r))
      => LastMember end (eff ': r) where
  decompLast (Union SZ u)     = Left $ Union SZ u
  decompLast (Union (SS n) u) = first weaken $ decompLast $ Union n u

instance LastMember end '[end] where
  decompLast = Right

