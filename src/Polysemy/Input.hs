{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Input where

import Data.Foldable (for_)
import Data.List (uncons)
import Polysemy
import Polysemy.State

data Input i m a where
  Input :: Input i m i

makeSemantic ''Input


runConstInput :: i -> Semantic (Input i ': r) a -> Semantic r a
runConstInput c = interpret \case
  Input -> pure c
{-# INLINE runConstInput #-}


runListInput
    :: Typeable i
    => [i]
    -> Semantic (Input (Maybe i) ': r) a
    -> Semantic r a
runListInput is = fmap snd . runState is . reinterpret \case
  Input -> do
    s <- gets uncons
    for_ s $ put . snd
    pure $ fmap fst s
{-# INLINE runListInput #-}


runMonadicInput :: Semantic r i -> Semantic (Input i ': r) a -> Semantic r a
runMonadicInput m = interpret \case
  Input -> m
{-# INLINE runMonadicInput #-}

