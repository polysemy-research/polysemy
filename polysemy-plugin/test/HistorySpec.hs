{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin -fconstraint-solver-iterations=10 #-}

module HistorySpec where

import Polysemy
import Polysemy.State
import Test.Hspec

data History s m a where
  Undo :: History s m ()
  Redo :: History s m ()
  PutAndForget :: s -> History s m ()

makeSem ''History


spec :: Spec
spec = describe "HistorySpec" $ it "should compile" $ do
  let z = run . runState (0 :: Int) . runHistory $ do
            modify (+ 5)
            undo
            x <- get
            redo
            y <- get
            undo
            put 1
            redo
            pure (x, y)
  z `shouldBe` (1 :: Int, (0 :: Int, 5 :: Int))


data Zipper a = Zipper [a] a [a]

focusZ :: Zipper a -> a
focusZ (Zipper _ a _) = a

modifyZ :: (a -> a) -> Zipper a -> Zipper a
modifyZ f (Zipper past a _) = Zipper (a : past) (f a) []

goBack :: Zipper a -> Zipper a
goBack z@(Zipper [] _ _) = z
goBack (Zipper (p : ps) a fs) = Zipper ps p (a : fs)

goForward :: Zipper a -> Zipper a
goForward z@(Zipper _ _ []) = z
goForward (Zipper ps a (f : fs)) = Zipper (a : ps) f fs

runHistory
    :: forall s r a
     . Member (State s) r
    => Sem (History s ': r) a
    -> Sem r a
runHistory m = do
  s <- get
  evalState (Zipper [] s [])
    . interpret \case
        Undo -> do
          s' <- modify goBack >> gets focusZ
          put s'
        Redo -> do
          s' <- modify goForward >> gets focusZ
          put s'
        PutAndForget s' ->
          put s'
    . intercept @(State s) \case
        Get -> get
        Put s' -> do
          modify $ modifyZ $ const s'
          put s'
    . raiseUnder @(State (Zipper s))
    $ m

