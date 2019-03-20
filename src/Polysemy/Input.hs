{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Input where

import Data.Foldable (for_)
import Data.List (uncons)
import Polysemy
import Polysemy.Effect.New
import Polysemy.Reader
import Polysemy.State

newtype Input i m a = Input (i -> a)
  deriving (Functor, Effect)

makeSemantic ''Input


runConstInput :: i -> Semantic (Input i ': r) a -> Semantic r a
runConstInput c = interpret \case
  Input k -> pure $ k c
{-# INLINE runConstInput #-}


runListInput :: [i] -> Semantic (Input (Maybe i) ': r) a -> Semantic r a
runListInput is = fmap snd . runState is . reinterpret \case
  Input k -> do
    s <- gets uncons
    for_ s $ put . snd
    pure $ k $ fmap fst s
{-# INLINE runListInput #-}


runMonadicInput :: Semantic r i -> Semantic (Input i ': r) a -> Semantic r a
runMonadicInput m = interpret \case
  Input k -> fmap k m
{-# INLINE runMonadicInput #-}


runInputAsReader :: Semantic (Input i ': r) a -> Semantic (Reader i ': r) a
runInputAsReader = reinterpret \case
  Input k -> fmap k ask
{-# INLINE runInputAsReader #-}

