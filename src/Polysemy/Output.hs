{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Output where

import Polysemy
import Polysemy.Effect.New
import Polysemy.State

data Output o m a = Output o a
  deriving (Functor, Effect)

makeSemantic ''Output


runFoldMapOutput
    :: Monoid m
    => (o -> m)
    -> Semantic (Output o ': r) a
    -> Semantic r (m, a)
runFoldMapOutput f = runState mempty . reinterpret \case
  Output o k -> do
    modify (<> f o)
    pure k
{-# INLINE runFoldMapOutput #-}


runIgnoringOutput
    :: Semantic (Output o ': r) a
    -> Semantic r a
runIgnoringOutput = interpret \case
  Output _ k -> pure k
{-# INLINE runIgnoringOutput #-}

