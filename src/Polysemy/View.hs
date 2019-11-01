{-# LANGUAGE TemplateHaskell #-}

module Polysemy.View
  ( -- * Effect
    View (..)

    -- * Actions
  , see

    -- * Interpretations
  , viewToState
  , viewToInput
  ) where

import Polysemy
import Polysemy.Input
import Polysemy.State
import Polysemy.Tagged


------------------------------------------------------------------------------
-- | A 'View' is an expensive computation that should be cached.
data View v m a where
  See :: View v m v

makeSem ''View


------------------------------------------------------------------------------
-- | Transform a 'View' into an 'Input'.
viewToInput
    :: forall v i r a
     . Member (Input i) r
    => (i -> v)
    -> Sem (View v ': r) a
    -> Sem r a
viewToInput f = interpret $ \case
  See -> f <$> input


------------------------------------------------------------------------------
-- | Get a 'View' as an exensive computation over an underlying 'State' effect.
-- This 'View' is only invalidated when the underlying 'State' changes.
viewToState
    :: forall v s r a
     . Member (State s) r
    => (s -> Sem r v)
    -> Sem (View v ': r) a
    -> Sem r a
viewToState f = do
  evalState Dirty
    . untag @"view" @(State (Cached v))
    . intercept @(State s)
      ( \case
        Get -> get
        Put s -> do
          put s
          tag @"view" @(State (Cached v)) $ put $ Dirty @v
      )
    . reinterpret @(View v)
      ( \case
          See -> do
            dirty <- tagged @"view" $ get @(Cached v)
            case dirty of
              Dirty -> do
                s <- get
                v' <- raise $ f s
                tagged @"view" $ put $ Cached v'
                pure v'
              Cached v -> pure v
      )


data Cached a = Cached a | Dirty
  deriving (Eq, Ord, Show, Functor)

