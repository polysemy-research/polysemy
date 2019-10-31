{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Polysemy.Law
  ( Law (..)
  , runLaw
  , Citizen (..)
  , module Test.QuickCheck
  ) where

import Control.Arrow (first)
import Data.Char
import Polysemy
import Test.QuickCheck


class Citizen r a | r -> a where
  getCitizen :: r -> r -> Gen ([String], (a, a))


instance {-# OVERLAPPING #-} Citizen (Sem r a -> b) (Sem r a -> b) where
  getCitizen r1 r2 = pure ([], (r1, r2))

instance {-# OVERLAPPING #-} Citizen (Sem r a) (Sem r a) where
  getCitizen r1 r2 = pure ([], (r1, r2))

instance (Arbitrary a, Show a, Citizen b r) => Citizen (a -> b) r where
  getCitizen f1 f2 = do
    a <- arbitrary
    first (show a :) <$> getCitizen (f1 a) (f2 a)


data Law e r where
  Law
      :: ( Eq a
         , Show a
         , Citizen i12n (Sem r x -> a)
         , Citizen res (Sem (e ': r) x)
         )
      => i12n
      -> String
      -> res
      -> String
      -> res
      -> Law e r
  LawIO
      :: ( Eq a
         , Show a
         , Citizen i12n (Sem r x -> IO a)
         , Citizen res (Sem (e ': r) x)
         )
      => i12n
      -> String
      -> res
      -> String
      -> res
      -> Law e r


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

printf :: String -> [String] -> String
printf str args = splitArgs str
  where
    splitArgs :: String -> String
    splitArgs s =
      case break (== '%') s of
        (as, "") -> as
        (as, drop 1 -> (b : bs))
          | isDigit b
          , let d = read [b] - 1
          , d < length args
            -> as ++ (args !! d) ++ splitArgs bs
        (as, drop 1 -> bs) ->  as ++ "%" ++ splitArgs bs

