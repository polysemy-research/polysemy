{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Singleton list
module Polysemy.Internal.Sing where

import GHC.TypeLits (Nat, type (-))

import Polysemy.Internal.Kind (Effect)

------------------------------------------------------------------------------
-- | A singleton type used as a witness for type-level lists.
data SList l where
  SEnd  :: SList '[]
  SCons :: SList xs -> SList (x ': xs)

------------------------------------------------------------------------------
-- | A singleton list constructor class.
class KnownList l where
  singList :: SList l

instance KnownList '[] where
  singList = SEnd
  {-# INLINE singList #-}

instance KnownList xs => KnownList (x ': xs) where
  singList = SCons singList
  {-# INLINE singList #-}

------------------------------------------------------------------------------
-- | A utility class for constructing a type-level list of a given length.
class ListOfLength (n :: Nat) (l :: [Effect]) where
  listOfLength :: SList l

instance {-# OVERLAPPING #-} (l ~ '[]) => ListOfLength 0 l where
  listOfLength = SEnd
  {-# INLINE listOfLength #-}

instance (ListOfLength (n - 1) xs, l ~ (x ': xs)) => ListOfLength n l where
  listOfLength = SCons (listOfLength @(n - 1))
  {-# INLINE listOfLength #-}
