{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Output where

import Polysemy
import Polysemy.State


data Output o m a where
  Output :: o -> Output o m ()

makeSemantic ''Output


runFoldMapOutput
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Semantic (Output o ': r) a
    -> Semantic r (m, a)
runFoldMapOutput f = runState mempty . reinterpret \case
  Output o -> modify (<> f o)
{-# INLINE runFoldMapOutput #-}


runIgnoringOutput :: Semantic (Output o ': r) a -> Semantic r a
runIgnoringOutput = interpret \case
  Output _ -> pure ()
{-# INLINE runIgnoringOutput #-}

