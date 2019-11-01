{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Polysemy.History
  ( -- * Effect
    History (..)

    -- * Actions
  , undo
  , redo
  , putAndForget

    -- * Interpretations
  , runHistory
  ) where

import Polysemy
import Polysemy.State
import Polysemy.Tagged


data History s m a where
  Undo :: History s m ()
  Redo :: History s m ()
  PutAndForget :: s -> History s m ()

makeSem ''History


runHistory
    :: forall s r a
     . Member (State s) r
    => Sem (History s ': r) a
    -> Sem r a
runHistory m = do
  s <- get
  evalState (Zipper [] s [])
    . untag @"history" @(State (Zipper s))
    . interpret @(History s) (\case
        Undo -> do
          s' <- tagged @"history" $ modify (goBack @s) >> gets (focusZ @s)
          put s'
        Redo -> do
          s' <- tagged @"history" $ modify (goForward @s) >> gets (focusZ @s)
          put s'
        PutAndForget s' ->
          put s'
                             )
    . intercept @(State s) ( \case
        Get -> get
        Put s' -> do
          raise $ tagged @"history" $ modify $ modifyZ $ const s'
          put s'
                           )
    . raiseUnder
    $ m


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

