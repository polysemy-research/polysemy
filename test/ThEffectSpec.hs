{-# LANGUAGE TemplateHaskell, GADTs, DataKinds, AllowAmbiguousTypes #-}

module ThEffectSpec where

import Polysemy
import Test.Hspec
import GHC.TypeLits
import Data.Kind
import Language.Haskell.TH hiding (Type)

spec :: Spec
spec = parallel $ it "should compile" True

-- Infix effects and actions -------------------------------------------------

data (:#) m a where
  (:#) :: a -> b -> m :# a

infixl 4 :#

makeSem ''(:#)

reifyFixity '(#) >>= \case
  Just (Fixity 4 InfixL) -> return []
  _                      -> fail "Wrong fixity of generated operator"

-- ADTs and ADT syntax -------------------------------------------------------

data SimpleADT m a = SimpleADTC1 Int | SimpleADTC2 String

makeSem ''SimpleADT

data GADTSyntax m a where
  GADTSyntaxC1 :: Int -> GADTSyntax m a
  GADTSyntaxC2 :: String -> GADTSyntax m a

makeSem ''GADTSyntax

data ADTSyntax1 m a = a ~ Int => ADTSyntax1C String

makeSem ''ADTSyntax1

data ADTSyntax2 m a
  = a ~ Int    => ADTSyntax2C1 Int
  | a ~ String => ADTSyntax2C2 String

makeSem ''ADTSyntax2

data ADTSyntax3 m a = Show a => ADTSyntax3C a

makeSem ''ADTSyntax3

-- We don't care about named fields (except that we accept them as names from
-- effect in 'makeSem')
data Fields m a = FieldsC { fieldsCF1 :: Int, fieldsCF2 :: String }

makeSem ''Fields

-- Newtypes ------------------------------------------------------------------

newtype Newtype1 m a = Newtype1C Int

makeSem ''Newtype1

newtype Newtype2 m a where
  Newtype2C :: String -> Newtype2 m a

makeSem ''Newtype2

-- Data families -------------------------------------------------------------

data Instance = ADTI | GADTI | NTI | MMI

data family Family (s :: Instance) (m :: Type -> Type) a

data instance Family 'ADTI m a = ADTIC1 Int | ADTIC2 String

makeSem 'ADTIC1

data instance Family 'GADTI m a where
  GADTIC1 :: Int -> Family 'GADTI m Int
  GADTIC2 :: String -> Family 'GADTI m String

makeSem 'GADTIC1

newtype instance Family 'NTI m a = NTIC Int

makeSem 'NTIC

data instance Family 'MMI m (f m) where
  MMIC1 :: f m -> Family 'MMI m (f m)
  MMIC2 :: (forall x. m x -> m (f m)) -> Family 'MMI m (f m)

makeSem 'MMIC1

-- Phantom types -------------------------------------------------------------

data Phantom m a

makeSem ''Phantom

-- Complex action types ------------------------------------------------------

-- Inspired by:
-- github.com/lexi-lambda/freer-simple/blob/ec84ae4e23ccba1ae05368100da750c196bbbcbb/tests/Tests/TH.hs#L37
data Complex m a where
  Mono            :: Int -> Complex m Bool
  Poly            :: a -> Complex m a
  PolyIn          :: a -> Complex m Bool
  PolyOut         :: Int -> Complex m a
  Lots            :: a -> b -> c -> d -> e -> f -> Complex m ()
  Nested          :: Maybe b -> Complex m (Maybe a)
  MultiNested     :: (Maybe a, [b]) -> Complex m (Maybe a, [b])
  Existential     :: (forall e. e -> Maybe e) -> Complex m a
  LotsNested      :: Maybe a -> [b] -> (c, c) -> Complex m (a, b, c)
  Dict            :: Ord a => a -> Complex m a
  MultiDict       :: (Eq a, Ord b, Enum a, Num c)
                  => a -> b -> c -> Complex m ()
  IndexedMono     :: f 0 -> Complex m Int
  IndexedPoly     :: forall f (n :: Nat) m . f n -> Complex m (f (n + 1))
  IndexedPolyDict :: KnownNat n => f n -> Complex m Int

makeSem ''Complex

data HOEff m a where
  EffArgMono :: m () -> HOEff m ()
  EffArgPoly :: m a -> HOEff m a
  EffArgComb :: m a -> (m a -> m b) -> HOEff m b
  EffRank2   :: (forall x. m x -> m (Maybe x)) -> HOEff m a

makeSem ''HOEff

data ComplexEffArgs b c m a where
  EffMono     :: Int -> ComplexEffArgs Int String m Bool
  EffPoly1    :: a -> ComplexEffArgs a b m a
  EffPoly2    :: a -> ComplexEffArgs a (Maybe a) m Bool
  EffPolyFree :: String -> ComplexEffArgs a b m Int
  EffSame1    :: ComplexEffArgs a a m a
  EffSame2    :: ComplexEffArgs b b m a
  EffHO       :: m b -> ComplexEffArgs b Int m String

makeSem ''ComplexEffArgs

data HKEffArgs f g m a where
  HKRank2 :: (forall x . f x -> g x) -> HKEffArgs f g m a

makeSem ''HKEffArgs

-- 'makeSem' input names -----------------------------------------------------

data ByCon m a where
  ByConC :: Int -> ByCon m String

makeSem 'ByConC

data ByField m a where
  ByFieldC :: { byFieldCF :: Int } -> ByField m Int

makeSem 'byFieldCF
