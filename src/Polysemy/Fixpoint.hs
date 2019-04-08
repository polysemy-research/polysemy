{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Fixpoint
  ( Fixpoint ()
  , module Polysemy.Fixpoint
  ) where

import Control.Monad.Fix
import Polysemy
import Polysemy.Effect.New
import Polysemy.Fixpoint.Type
import Polysemy.Union


runFixpoint
    :: (∀ x. Semantic r x -> x)
    -> Semantic (Fixpoint ': r) a
    -> Semantic r a
runFixpoint lower (Semantic m) = Semantic $ \k -> m $ \u ->
  case decomp u of
    Left x -> k $ hoist (runFixpoint lower) x
    Right (Yo (Fixpoint mf) _ d y) ->
      usingSemantic k
        . fmap y
        . pure
        . fix
        . fmap (lower . runFixpoint lower)
        $ d . fmap mf

runFixpointM
    :: ( MonadFix m
       , Member (Lift m) r
       )
    => (∀ x. Semantic r x -> m x)
    -> Semantic (Fixpoint ': r) a
    -> Semantic r a
runFixpointM lower (Semantic m) = Semantic $ \k -> m $ \u ->
  case decomp u of
    Left x -> k $ hoist (runFixpointM lower) x
    Right (Yo (Fixpoint mf) _ d y) ->
      usingSemantic k
        . fmap y
        . sendM
        . mfix
        . fmap (lower . runFixpointM lower)
        $ d . fmap mf

