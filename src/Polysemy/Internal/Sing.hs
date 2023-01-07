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
  , ListOfLength
  , listOfLength
  ) where

import GHC.TypeLits (Nat, Symbol, type (-), natVal', KnownNat)
import GHC.Exts (proxy#)
import Type.Errors (TypeError)
import Unsafe.Coerce

import Polysemy.Internal.Kind (Effect)
import Polysemy.Internal.CustomErrors

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

instance KnownList xs => KnownList (x ': xs) where
  singList = SCons singList

------------------------------------------------------------------------------
-- | A utility class for constructing a type-level list of a given length.
class (KnownNat n, KnownList l)
   => ListOfLength' (stuck :: ()) (s :: Symbol) (n :: Nat) (l :: [Effect]) where
  listOfLength' :: SList l
  listOfLength' = UnsafeMkSList (fromInteger (natVal' (proxy# @n)))

instance {-# OVERLAPPING #-} (l ~ '[]) => ListOfLength' '() s 0 l
instance (KnownNat n, ListOfLength' '() s (n - 1) xs, l ~ (x ': xs))
      => ListOfLength' '() s n l
instance {-# INCOHERENT #-} (UnprovidedIndex s, KnownNat n, KnownList l)
      => ListOfLength' stuck s n l

class    (ListOfLength' (StuckCheck n) s n l) => ListOfLength s n l
instance (ListOfLength' (StuckCheck n) s n l) => ListOfLength s n l

listOfLength :: forall s n l. ListOfLength s n l => SList l
listOfLength = listOfLength' @(StuckCheck n) @s @n @l

type family UnprovidedIndex name where
  UnprovidedIndex name = TypeError (
    name <> ": You must provide the length of the prefix as a type application."
    % "Example: " <> name <> " @5"
    )
