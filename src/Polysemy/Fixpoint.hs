{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Fixpoint
  ( Fixpoint ()
  , module Polysemy.Fixpoint
  ) where

import Control.Monad.Fix
import Polysemy
import Polysemy.Effect.New
import Polysemy.Fixpoint.Type


inlineRecursiveCalls [d|
  runFixpoint
      :: (∀ x. Semantic r x -> x)
      -> Semantic (Fixpoint ': r) a
      -> Semantic r a
  runFixpoint lower = interpret $ \case
    Fixpoint f k ->
      pure $ k $ fix $ lower . runFixpoint lower . f


  runFixpointM
      :: ( MonadFix m
         , Member (Lift m) r
         )
      => (∀ x. Semantic r x -> m x)
      -> Semantic (Fixpoint ': r) a
      -> Semantic r a
  runFixpointM lower = interpret $ \case
    Fixpoint f k ->
      fmap k $ sendM $ mfix $ lower . runFixpointM lower . f
  |]

