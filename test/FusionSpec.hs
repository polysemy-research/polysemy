{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -O2 #-}

module FusionSpec where

import Test.Inspection
import Control.Monad.Discount
import Data.OpenUnion
import TRYAGAIN hiding (main)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Except as E
import Test.Hspec


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure e) = error e

shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` isSuccess


spec :: Spec
spec = do
  describe "fusion" $ do
    it "Union proofs should simplify" $ do
      shouldSucceed $(inspectTest $ 'countDown `hasNoType` ''SNat)

    it "internal uses of StateT should simplify" $ do
      shouldSucceed $(inspectTest $ 'countDown `doesNotUse` ''S.StateT)
      shouldSucceed $(inspectTest $ 'jank      `doesNotUse` ''S.StateT)

    it "internal uses of ExceptT should simplify" $ do
      shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` ''E.ExceptT)

    it "`runState . reinterpret` should fuse" $ do
      shouldSucceed $(inspectTest $ 'jank      `doesNotUse` 'reinterpret)
      shouldSucceed $(inspectTest $ 'jank      `doesNotUse` 'hoist)

go :: Eff '[State Int] Int
go = do
  n <- send (Get id)
  if n <= 0
     then pure n
     else do
       send $ Put (n-1) ()
       go

tryIt :: Either Bool String
tryIt = run . runError @Bool $ do
  catch @Bool
    do
      throw False
    \_ -> pure "hello"

countDown :: Int -> Int
countDown start = fst $ run $ runState start go

jank :: Int -> Int
jank start = fst $ run $ runState start $ reinterpret send $ go

