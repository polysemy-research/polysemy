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

-- |
-- Module:       Data.OpenUnion.Internal
-- Description:  Open unions (type-indexed co-products) for extensible effects.
--
-- Copyright:    (c) 2016 Allele Dev; 2017 Ixperta Solutions s.r.o.; 2017 Alexis King
-- License:      BSD3
-- Maintainer:   Alexis King <lexi.lambda@gmail.com>
-- Stability:    experimental
-- Portability:  GHC specific language extensions.
--
-- These are internal definitions and should be used with caution. There are no
-- guarantees that the API of this module will be preserved between minor
-- versions of this package.
--
-- Open unions (type-indexed co-products, i.e. type-indexed sums) for
-- extensible effects All operations are constant-time.
--
-- Based on
-- <http://okmij.org/ftp/Haskell/extensible/OpenUnion51.hs OpenUnion51.hs>.
--
-- Type-list @r :: [* -> *]@ of open union components is a small Universe.
-- Therefore, we can use a @Typeable@-like evidence in that universe. In our
-- case a simple index of an element in the type-list is sufficient
-- substitution for @Typeable@.
module Data.OpenUnion.Internal where

import GHC.TypeLits (TypeError, ErrorMessage(..))
import Unsafe.Coerce (unsafeCoerce)
import Data.Functor.Identity
import Data.Functor.Compose


------------------------------------------------------------------------------
-- | A natural transformation from 'f' to 'g' (that is, @forall x. f x -> g x@)
type f ~> g = forall x. f x -> g x
infixr 1 ~>

class HFunctor t where
  hoist :: Monad g => (f ~> g) -> t f ~> t g


-- | Open union is a strong sum (existential with an evidence).
data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union :: {-# UNPACK #-} !Word -> Yo t m a -> Union r m a

instance HFunctor (Union r) where
  hoist f (Union w t) = Union w $ hoist f t
  {-# INLINE hoist #-}


type (.:) f g a = f (g a)
infixr 9 .:


data Yo e m a where
  Yo :: (Monad m, Monad n, Functor tk)
     => e m a
     -> tk ()
     -> (tk .: m ~> n .: tk)
     -> (tk a -> b)
     -> Yo e n b

instance HFunctor (Yo e) where
  hoist f (Yo e s nt z) = Yo e s (f . nt) z
  {-# INLINE hoist #-}


weave
    :: (Monad m, Monad n, Functor f)
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> Union r m a
    -> Union r n (f a)
weave s' distrib (Union w (Yo e s nt f)) =
  Union w $
    Yo e (Compose $ s <$ s')
         (fmap Compose . distrib . fmap nt . getCompose)
         (fmap f . getCompose)
{-# INLINE weave #-}



freeYo :: Monad m => e m a -> Yo e m a
freeYo e = Yo e (Identity ()) (fmap Identity . runIdentity) runIdentity
{-# INLINE freeYo #-}


-- | Takes a request of type @t :: * -> *@, and injects it into the 'Union'.
--
-- Summand is assigning a specified 'Word' value, which is a position in the
-- type-list @(t ': r) :: * -> *@.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafeInj :: Monad m => Word -> t m a -> Union r m a
unsafeInj w = Union w . freeYo
{-# INLINE unsafeInj #-}

-- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
-- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is not
-- the value stored in the @'Union' (t ': r) :: * -> *@.
--
-- It is assumed that summand is stored in the 'Union' when the 'Word' value is
-- the same value as is stored in the 'Union'.
--
-- __This function is unsafe.__
--
-- /O(1)/
unsafePrj :: Word -> Union r m a -> Maybe (t m a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

-- | Represents position of element @t :: * -> *@ in a type list
-- @r :: [* -> *]@.
newtype P t r = P {unP :: Word}

-- | Find an index of an element @t :: * -> *@ in a type list @r :: [* -> *]@.
-- The element must exist. The @w :: [* -> *]@ type represents the entire list,
-- prior to recursion, and it is used to produce better type errors.
--
-- This is essentially a compile-time computation without run-time overhead.
class FindElem (t :: k) (r :: [k]) where
  -- | Position of the element @t :: * -> *@ in a type list @r :: [* -> *]@.
  --
  -- Position is computed during compilation, i.e. there is no run-time
  -- overhead.
  --
  -- /O(1)/
  elemNo :: P t r

-- | Base case; element is at the current position in the list.
instance FindElem t (t ': r) where
  elemNo = P 0

-- | Recursion; element is not at the current position, but is somewhere in the
-- list.
instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

-- | Instance resolution for this class fails with a custom type error
-- if @t :: * -> *@ is not in the list @r :: [* -> *]@.
class IfNotFound (t :: k) (r :: [k]) (w :: [k])

-- | If we reach an empty list, that’s a failure, since it means the type isn’t
-- in the list. For GHC >=8, we can render a custom type error that explicitly
-- states what went wrong.
instance TypeError ('Text "‘" ':<>: 'ShowType t
                    ':<>: 'Text "’ is not a member of the type-level list"
                    ':$$: 'Text "  ‘" ':<>: 'ShowType w ':<>: 'Text "’"
                    ':$$: 'Text "In the constraint ("
                    ':<>: 'ShowType (Member t w) ':<>: 'Text ")")
    => IfNotFound t '[] w

instance IfNotFound t (t ': r) w
instance {-# OVERLAPPABLE #-} IfNotFound t r w => IfNotFound t (t' ': r) w

-- | Pass if @r@ is uninstantiated. The incoherence here is safe, since picking
-- this instance doesn’t cause any variation in behavior, except possibly the
-- production of an inferior error message. For more information, see
-- lexi-lambda/freer-simple#3, which describes the motivation in more detail.
instance {-# INCOHERENT #-} IfNotFound t r w

-- | A constraint that requires that a particular effect, @eff@, is a member of
-- the type-level list @effs@. This is used to parameterize an
-- 'Control.Monad.Freer.Eff' computation over an arbitrary list of effects, so
-- long as @eff@ is /somewhere/ in the list.
--
-- For example, a computation that only needs access to a cell of mutable state
-- containing an 'Integer' would likely use the following type:
--
-- @
-- 'Member' ('Control.Monad.Freer.State.State' 'Integer') effs => 'Control.Monad.Freer.Eff' effs ()
-- @
class FindElem eff effs => Member (eff :: (* -> *) -> * -> *) (effs :: [(* -> *) -> * -> *]) where
  -- This type class is used for two following purposes:
  --
  -- * As a @Constraint@ it guarantees that @t :: * -> *@ is a member of a
  --   type-list @r :: [* -> *]@.
  --
  -- * Provides a way how to inject\/project @t :: * -> *@ into\/from a 'Union',
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
  inj :: Monad m => eff m a -> Union effs m a

  -- | Project a value of type @'Union' (t ': r) :: * -> *@ into a possible
  -- summand of the type @t :: * -> *@. 'Nothing' means that @t :: * -> *@ is
  -- not the value stored in the @'Union' (t ': r) :: * -> *@.
  --
  -- /O(1)/
  prj :: Union effs m a -> Maybe (eff m a)

instance (FindElem t r, IfNotFound t r r) => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  {-# INLINE inj #-}

  prj = unsafePrj $ unP (elemNo :: P t r)
  {-# INLINE prj #-}

-- | Orthogonal decomposition of a @'Union' (t ': r) :: * -> *@. 'Right' value
-- is returned if the @'Union' (t ': r) :: * -> *@ contains @t :: * -> *@, and
-- 'Left' when it doesn't. Notice that 'Left' value contains
-- @Union r :: * -> *@, i.e. it can not contain @t :: * -> *@.
--
-- /O(1)/
decomp :: Union (t ': r) m a -> Either (Union r m a) (Yo t m a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}

-- | Specialized version of 'decomp' for efficiency.
--
-- /O(1)/
--
-- TODO: Check that it actually adds on efficiency.
decomp0 :: Union '[t] m a -> Either (Union '[] m a) (Yo t m a)
decomp0 (Union _ a) = Right $ unsafeCoerce a
{-# INLINE decomp0 #-}
{-# RULES "decomp/singleton"  decomp = decomp0 #-}

-- | Specialised version of 'prj'\/'decomp' that works on an
-- @'Union' '[t] :: * -> *@ which contains only one specific summand. Hence the
-- absence of 'Maybe', and 'Either'.
--
-- /O(1)/
extract :: Union '[t] m a -> Yo t m a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}

-- | Introduce a type directly under the head of the 'Union'.
intro1 :: Union (u ': r) m a -> Union (u ': v ': r) m a
intro1 (Union 0 a) = Union 0 a
intro1 (Union n a) = Union (n + 1) a
{-# INLINE intro1 #-}

-- | Introduce two types directly under the head of the 'Union'.
intro2 :: Union (u ': r) m a -> Union (u ': v ': x ': r) m a
intro2 (Union 0 a) = Union 0 a
intro2 (Union n a) = Union (n + 2) a
{-# INLINE intro2 #-}

-- | Introduce three type directly under the head of the 'Union'.
intro3 :: Union (u ': r) m a -> Union (u ': v ': x ': y ': r) m a
intro3 (Union 0 a) = Union 0 a
intro3 (Union n a) = Union (n + 3) a
{-# INLINE intro3 #-}

-- | Introduce three type directly under the head of the 'Union'.
intro4 :: Union (u ': r) m a -> Union (u ': v ': x ': y ': z ': r) m a
intro4 (Union 0 a) = Union 0 a
intro4 (Union n a) = Union (n + 4) a
{-# INLINE intro4 #-}


infixr 5 :++:
type family xs :++: ys where
  '[] :++: ys = ys
  (x ': xs) :++: ys = x ': (xs :++: ys)

-- class Weakens q where
--   weakens :: Union r m a -> Union (q :++: r) m a

-- instance Weakens '[] where
--   weakens = id
--   {-# INLINE weakens #-}

-- instance Weakens xs => Weakens (x ': xs) where
--   weakens u = weaken (weakens @xs u)
--   {-# INLINEABLE weakens #-}
