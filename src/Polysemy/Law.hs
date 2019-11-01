{-# LANGUAGE CPP                    #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

#if __GLASGOW_HASKELL__ < 806
-- There is a bug in older versions of Haddock that don't allow documentation
-- on GADT arguments.
#define HADDOCK --
#else
#define HADDOCK -- ^
#endif

module Polysemy.Law
  ( Law (..)
  , runLaw
  , MakeLaw (..)
  , Citizen (..)
  , printf
  , module Test.QuickCheck
  ) where

import Control.Arrow (first)
import Data.Char
import Polysemy
import Test.QuickCheck


------------------------------------------------------------------------------
-- | Associates the name @r@ with the eventual type @a@. For example,
-- @'Citizen' (String -> Bool) Bool@ can produce arbitrary @Bool@s by calling
-- the given function with arbitrary @String@s.
class Citizen r a | r -> a where
  -- | Generate two @a@s via two @r@s. Additionally, produce a list of strings
  -- corresponding to any arbitrary arguments we needed to build.
  getCitizen :: r -> r -> Gen ([String], (a, a))

instance {-# OVERLAPPING #-} Citizen (Sem r a -> b) (Sem r a -> b) where
  getCitizen r1 r2 = pure ([], (r1, r2))

instance Citizen (Sem r a) (Sem r a) where
  getCitizen r1 r2 = pure ([], (r1, r2))

instance (Arbitrary a, Show a, Citizen b r) => Citizen (a -> b) r where
  getCitizen f1 f2 = do
    a <- arbitrary
    first (show a :) <$> getCitizen (f1 a) (f2 a)


------------------------------------------------------------------------------
-- | A law that effect @e@ must satisfy whenever it is in environment @r@. You
-- can use 'runLaw' to transform these 'Law's into QuickCheck-able 'Property's.
data Law e r where
  -- | A pure 'Law', that doesn't require any access to 'IO'.
  Law
      :: ( Eq a
         , Show a
         , Citizen i12n (Sem r x -> a)
         , Citizen res (Sem (e ': r) x)
         )
      => i12n
         HADDOCK An interpretation from @'Sem' r x@ down to a pure value. This is
         -- likely 'run'.
      -> String
         HADDOCK A string representation of the left-hand of the rule. This is
         -- a formatted string, for more details, refer to 'printf'.
      -> res
         HADDOCK The left-hand rule. This thing may be of type @'Sem' (e ': r) x@,
         -- or be a function type that reproduces a @'Sem' (e ': r) x@. If this
         -- is a function type, it's guaranteed to be called with the same
         -- arguments that the right-handed side was called with.
      -> String
         HADDOCK A string representation of the right-hand of the rule. This is
         -- a formatted string, for more details, refer to 'printf'.
      -> res
         HADDOCK The right-hand rule. This thing may be of type @'Sem' (e ': r) x@,
         -- or be a function type that reproduces a @'Sem' (e ': r) x@. If this
         -- is a function type, it's guaranteed to be called with the same
         -- arguments that the left-handed side was called with.
      -> Law e r
  -- | Like 'Law', but for 'IO'-accessing effects.
  LawIO
      :: ( Eq a
         , Show a
         , Citizen i12n (Sem r x -> IO a)
         , Citizen res (Sem (e ': r) x)
         )
      => i12n
         HADDOCK An interpretation from @'Sem' r x@ down to an 'IO' value. This is
         -- likely 'runM'.
      -> String
         HADDOCK A string representation of the left-hand of the rule. This is
         -- a formatted string, for more details, refer to 'printf'.
      -> res
         HADDOCK The left-hand rule. This thing may be of type @'Sem' (e ': r) x@,
         -- or be a function type that reproduces a @'Sem' (e ': r) x@. If this
         -- is a function type, it's guaranteed to be called with the same
         -- arguments that the right-handed side was called with.
      -> String
         HADDOCK A string representation of the right-hand of the rule. This is
         -- a formatted string, for more details, refer to 'printf'.
      -> res
         HADDOCK The right-hand rule. This thing may be of type @'Sem' (e ': r) x@,
         -- or be a function type that reproduces a @'Sem' (e ': r) x@. If this
         -- is a function type, it's guaranteed to be called with the same
         -- arguments that the left-handed side was called with.
      -> Law e r


------------------------------------------------------------------------------
-- | A typeclass that provides the smart constructor 'mkLaw'.
class MakeLaw e r where
  -- | A smart constructor for building 'Law's.
  mkLaw
      :: (Eq a, Show a, Citizen res (Sem (e ': r) a))
      => String
      -> res
      -> String
      -> res
      -> Law e r

instance MakeLaw e '[] where
  mkLaw = Law run

instance MakeLaw e '[Embed IO] where
  mkLaw = LawIO runM


------------------------------------------------------------------------------
-- | Produces a QuickCheck-able 'Property' corresponding to whether the given
-- interpreter satisfies the 'Law'.
runLaw :: InterpreterFor e r -> Law e r -> Property
runLaw i12n (Law finish str1 a str2 b) = property $ do
  (_, (lower, _)) <- getCitizen finish finish
  (args, (ma, mb)) <- getCitizen a b
  let run_it = lower . i12n
      a' = run_it ma
      b' = run_it mb
  pure $
    counterexample
      (mkCounterexampleString str1 a' str2 b' args)
      (a' == b')
runLaw i12n (LawIO finish str1 a str2 b) = property $ do
  (_, (lower, _)) <- getCitizen finish finish
  (args, (ma, mb)) <- getCitizen a b
  let run_it = lower . i12n
  pure $ ioProperty $ do
    a' <- run_it ma
    b' <- run_it mb
    pure $
      counterexample
        (mkCounterexampleString str1 a' str2 b' args)
        (a' == b')


------------------------------------------------------------------------------
-- | Make a string representation for a failing 'runLaw' property.
mkCounterexampleString
    :: Show a
    => String
    -> a
    -> String
    -> a
    -> [String]
    -> String
mkCounterexampleString str1 a str2 b args =
  mconcat
    [ printf str1 args , " (result: " , show a , ")\n /= \n"
    , printf str2 args , " (result: " , show b , ")"
    ]


------------------------------------------------------------------------------
-- | A bare-boned implementation of printf. This function will replace tokens
-- of the form @"%n"@ in the first string with @args !! n@.
--
-- This will only work for indexes up to 9.
--
-- For example:
--
-- >>> printf "hello %1 %2% %3 %1" ["world", "50"]
-- "hello world 50% %3 world"
printf :: String -> [String] -> String
printf str args = splitArgs str
  where
    splitArgs :: String -> String
    splitArgs s =
      case break (== '%') s of
        (as, "") -> as
        (as, _ : b : bs)
          | isDigit b
          , let d = read [b] - 1
          , d < length args
            -> as ++ (args !! d) ++ splitArgs bs
        (as, _ : bs) ->  as ++ "%" ++ splitArgs bs

