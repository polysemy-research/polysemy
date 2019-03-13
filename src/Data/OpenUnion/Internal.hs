{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -Wno-redundant-constraints #-} -- Due to use of TypeError.
{-# OPTIONS_GHC -Wall #-} -- Due to use of TypeError.
{-# OPTIONS_HADDOCK not-home               #-}

--
--
--
--
--
module Data.OpenUnion.Internal where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Functor.Identity
import Data.Functor.Compose


------------------------------------------------------------------------------
type f ~> g = forall x. f x -> g x
infixr 1 ~>

class HFunctor t where
  hoist :: Monad g => (f ~> g) -> t f ~> t g


data Union (r :: [(* -> *) -> (* -> *) -> * -> *]) (s :: * -> *) (m :: * -> *) a where
  Union :: {-# UNPACK #-} !Word -> Yo t s m a -> Union r s m a

instance HFunctor (Union r tk) where
  hoist f (Union w t) = Union w $ hoist f t
  {-# INLINE hoist #-}


type (.:) f g a = f (g a)
infixr 9 .:


data Yo e (tk :: * -> *) m a where
  Yo :: (Monad m, Monad n, Functor tk)
     => e s m a
     -> tk ()
     -> (tk .: m ~> n .: tk)
     -> (tk a -> b)
     -> Yo e tk n b

instance HFunctor (Yo e tk) where
  hoist f (Yo e s nt z) = Yo e s (f . nt) z
  {-# INLINE hoist #-}


weave
    :: (Monad m, Monad n, Functor f)
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> Union r s m a
    -> Union r (Compose f s) n a
weave s' distrib (Union w (Yo e s nt f)) =
  Union w $
    Yo e (Compose $ s <$ s')
         (fmap Compose . distrib . fmap nt . getCompose)
         (fmap f . getCompose)
{-# INLINE weave #-}



freeYo :: Monad m => e Identity m a -> Yo e Identity m a
freeYo e = Yo e (Identity ()) (fmap Identity . runIdentity) runIdentity
{-# INLINE freeYo #-}


--
--
--
unsafeInj :: Monad m => Word -> t s m a -> Union r s m a
unsafeInj w = Union w . freeYo
{-# INLINE unsafeInj #-}

--
--
--
unsafePrj :: Word -> Union r s m a -> Maybe (t s m a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

newtype P t r = P {unP :: Word}

--
class FindElem (t :: k) (r :: [k]) where
  -- | Position of the element @t :: * -> *@ in a type list @r :: [* -> *]@.
  --
  -- Position is computed during compilation, i.e. there is no run-time
  -- overhead.
  --
  -- /O(1)/
  elemNo :: P t r

instance FindElem t (t ': r) where
  elemNo = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

class IfNotFound (t :: k) (r :: [k]) (w :: [k])

instance TypeError ('Text "‘" ':<>: 'ShowType t
                    ':<>: 'Text "’ is not a member of the type-level list"
                    ':$$: 'Text "  ‘" ':<>: 'ShowType w ':<>: 'Text "’"
                    ':$$: 'Text "In the constraint ("
                    ':<>: 'ShowType (Member t w) ':<>: 'Text ")")
    => IfNotFound t '[] w

instance IfNotFound t (t ': r) w
instance {-# OVERLAPPABLE #-} IfNotFound t r w => IfNotFound t (t' ': r) w

instance {-# INCOHERENT #-} IfNotFound t r w

--
--
class FindElem eff effs => Member (eff :: (* -> *) -> (* -> *) -> * -> *) (effs :: [(* -> *) -> (* -> *) -> * -> *]) where
  -- This type class is used for two following purposes:
  --
  -- * As a @Constraint@ it guarantees that @t :: * -> *@ is a member of a
  --   type-list @r :: [* -> *]@.
  --
  -- * Provides a way how to inject\/project @t :: * -> *@ into\/fros m a 'Union',
  --   respectively.
  --
  -- Following law has to hold:
  --
  -- @
  -- 'prj' . 'inj' === 'Just'
  -- @

  -- | Takes a request of type @t :: * -> *@, and injects it into the
  -- 'Union'.
  --
  -- /O(1)/
  inj :: Monad m => eff s m a -> Union effs s m a

  -- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
  -- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is
  -- not the value stored in the @'Union' (t ': r) :: * -> *@.
  --
  -- /O(1)/
  prj :: Union effs s m a -> Maybe (eff s m a)

instance (FindElem t r, IfNotFound t r r) => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  {-# INLINE inj #-}

  prj = unsafePrj $ unP (elemNo :: P t r)
  {-# INLINE prj #-}

--
decomp :: Union (t ': r) s m a -> Either (Union r s m a) (Yo t s m a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}

--
--
decomp0 :: Union '[t] s m a -> Either (Union '[] s m a) (Yo t s m a)
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}
{-# RULES "decomp/singleton"  decomp = decomp0 #-}

--
extract :: Union '[t] s m a -> Yo t s m a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

intro1 :: Union (u ': r) s m a -> Union (u ': v ': r) s m a
intro1 (Union 0 a) = Union 0 a
intro1 (Union n a) = Union (n + 1) a
{-# INLINE intro1 #-}

intro2 :: Union (u ': r) s m a -> Union (u ': v ': x ': r) s m a
intro2 (Union 0 a) = Union 0 a
intro2 (Union n a) = Union (n + 2) a
{-# INLINE intro2 #-}

intro3 :: Union (u ': r) s m a -> Union (u ': v ': x ': y ': r) s m a
intro3 (Union 0 a) = Union 0 a
intro3 (Union n a) = Union (n + 3) a
{-# INLINE intro3 #-}

intro4 :: Union (u ': r) s m a -> Union (u ': v ': x ': y ': z ': r) s m a
intro4 (Union 0 a) = Union 0 a
intro4 (Union n a) = Union (n + 4) a
{-# INLINE intro4 #-}


infixr 5 :++:
type family xs :++: ys where
  '[] :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

