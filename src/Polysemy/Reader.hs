{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Reader where

import Polysemy
import Polysemy.Effect.New
import Polysemy.Input

data Reader i m a where
  Ask :: Reader i m i
  Local :: (i -> i) -> m a -> Reader i m a

makeSemantic ''Reader


runReader :: i -> Semantic (Reader i ': r) a -> Semantic r a
runReader i = interpretH $ \case
  Ask -> begin i
  Local f m -> do
    mm <- start m
    raise $ runReader (f i) mm


runInputAsReader :: Semantic (Input i ': r) a -> Semantic (Reader i ': r) a
runInputAsReader = reinterpret $ \case
  Input -> ask
{-# INLINE runInputAsReader #-}

