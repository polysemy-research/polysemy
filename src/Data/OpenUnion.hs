{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module Data.OpenUnion where

import Control.Monad.Discount.Effect
import Unsafe.Coerce


data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union :: Effect e => Word -> e m a -> Union r m a


extract :: Union '[e] m a -> e m a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}


absurdU :: Union '[] m a -> b
absurdU = error "absurd, empty union"


unsafeInj :: Effect e => Word -> e m a -> Union r m a
unsafeInj w = Union w
{-# INLINE unsafeInj #-}


unsafePrj :: Word -> Union r m a -> Maybe (t m a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

newtype P t r = P {unP :: Word}


class FindElem (t :: k) (r :: [k]) where
  elemNo :: P t r


instance FindElem t (t ': r) where
  elemNo = P 0
  {-# INLINE elemNo #-}


instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)
  {-# INLINE elemNo #-}


class (FindElem e r, Effect e)
      => Member (e :: (* -> *) -> * -> *)
                (r :: [(* -> *) -> * -> *]) where
  inj :: Monad m => e m a -> Union r m a
  prj :: Union r m a -> Maybe (e m a)


instance (Effect t, FindElem t r) => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  {-# INLINE inj #-}

  prj = unsafePrj $ unP (elemNo :: P t r)
  {-# INLINE prj #-}


decomp :: Union (t ': r) m a -> Either (Union r m a) (e m a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}


weaken :: Union r m a -> Union (e ': r) m a
weaken (Union n a) = Union (n + 1) a


instance Effect (Union r) where
  weave s f (Union w e) = Union w $ weave s f e
  {-# INLINE weave #-}

instance Functor m => Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap f t
  {-# INLINE fmap #-}

