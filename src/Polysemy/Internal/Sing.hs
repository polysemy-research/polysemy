{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: Singleton list
module Polysemy.Internal.Sing
  ( SList(.., SCons, SEnd)
  , KnownList(..)
  , ListOfLength(..)
  ) where

import GHC.TypeLits (Nat, type (-), natVal', KnownNat)
import GHC.Exts (proxy#)
import Unsafe.Coerce

import Polysemy.Internal.Kind (Effect)

------------------------------------------------------------------------------
-- | A singleton type used as a witness for type-level lists.
type role SList nominal
newtype SList l = UnsafeMkSList Int

data MatchSCons l where
  MCYes :: SList xs -> MatchSCons (x ': xs)
  MCNo  :: MatchSCons l

data MatchSEnd l where
  MEYes :: MatchSEnd '[]
  MENo  :: MatchSEnd l

matchSEnd :: SList l -> MatchSEnd l
matchSEnd (UnsafeMkSList 0) = unsafeCoerce $ MEYes
matchSEnd _ = MENo

matchSCons :: SList l -> MatchSCons l
matchSCons (UnsafeMkSList 0) = MCNo
matchSCons (UnsafeMkSList e) = unsafeCoerce $ MCYes $ UnsafeMkSList $ e - 1

pattern SEnd :: () => (r ~ '[]) => SList r
pattern SEnd <- (matchSEnd -> MEYes)
  where
    SEnd = UnsafeMkSList 0

pattern SCons :: () => (l' ~ (x ': l)) => SList l -> SList l'
pattern SCons l <- (matchSCons -> MCYes l)
  where
    SCons (UnsafeMkSList sl) = UnsafeMkSList $ sl + 1

{-# COMPLETE SEnd, SCons #-}

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
class KnownNat n => ListOfLength (n :: Nat) (l :: [Effect]) where
  listOfLength :: SList l
  listOfLength = UnsafeMkSList (fromInteger (natVal' (proxy# @n)))

instance {-# OVERLAPPING #-} (l ~ '[]) => ListOfLength 0 l
instance (KnownNat n, ListOfLength (n - 1) xs, l ~ (x ': xs)) => ListOfLength n l
