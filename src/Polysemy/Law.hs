{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}
{-# LANGUAGE ViewPatterns           #-}

module Polysemy.Law where

import Data.Char
import Control.Applicative
import Control.Arrow ((&&&), first)
import Polysemy
import Polysemy.State
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


---

runStateLaws
    :: (Eq s, Show s, Arbitrary s)
    => InterpreterFor (State s) '[]
    -> Property
runStateLaws i12n = conjoin
  [ runLaw i12n lawPutTwice
  , runLaw i12n lawGetTwice
  , runLaw i12n lawGetPutGet
  ]

---


stateLaw
    :: (Eq a, Show a, Citizen res (Sem '[State s] a))
    => String
    -> res
    -> String
    -> res
    -> Law (State s) '[]
stateLaw = Law run


lawPutTwice
    :: (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawPutTwice =
  stateLaw
    "put %1 >> put %2 >> get"
    (\s s' -> put s >> put s' >> get)
    "put %2 >> get"
    (\s s' ->          put s' >> get)


lawGetTwice
    :: (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawGetTwice =
  stateLaw
    "liftA2 (,) get get"
    (liftA2 (,) get get)
    "(id &&& id) <$> get"
    ((id &&& id) <$> get)


lawGetPutGet
    :: (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawGetPutGet =
  stateLaw
    "get >>= put >> get"
    (get >>= put >> get)
    "get"
    get

---


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

