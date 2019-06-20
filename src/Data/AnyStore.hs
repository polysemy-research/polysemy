{-# LANGUAGE RoleAnnotations #-}

module Data.AnyStore
  ( AnyStore ()
  , Key ()
  , emptyS
  , allocS
  , getS
  , putS
  , modifyS
  , freeS
  , fiddleKey
  ) where


import qualified Data.IntMap.Strict as IM
import           Data.Kind
import           GHC.Exts
import           Unsafe.Coerce


data AnyStore (p :: Type) = AnyStore
  { _sNextId :: Int
  , _sData   :: IM.IntMap Any
  }

newtype Key (p :: Type) (a :: Type) = Key Int
  deriving (Eq, Ord)

fiddleKey :: Key p a -> Key p' a
fiddleKey (Key k) = Key k

type role Key phantom representational


emptyS :: AnyStore p
emptyS = AnyStore 0 IM.empty

allocS :: forall p s. s -> AnyStore p -> (AnyStore p, Key p s)
allocS s (AnyStore next d) =
  ( AnyStore (next + 1) $ IM.insert next (unsafeCoerce s) d
  , Key next
  )

getS :: Key p s -> AnyStore p -> s
getS (Key k) (AnyStore _ d)  = unsafeCoerce (d IM.! k)

putS :: Key p s -> s -> AnyStore p -> AnyStore p
putS (Key k) s (AnyStore n d) = AnyStore n $ IM.insert k (unsafeCoerce s) d

modifyS :: Key p s -> (s -> s) -> AnyStore p -> AnyStore p
modifyS k f d = putS k (f $! getS k d) d

freeS :: Key p s -> AnyStore p -> AnyStore p
freeS (Key k) (AnyStore n d) = AnyStore n $ IM.delete k d

