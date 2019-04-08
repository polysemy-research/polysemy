{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Polysemy.Writer where

import Polysemy
import Polysemy.Effect.New
import Polysemy.Output
import Polysemy.State

data Writer o m a where
  Tell :: o -> Writer o m ()
  Listen :: m a -> Writer o m (o, a)
  Censor :: (o -> o) -> m a -> Writer o m a

makeSemantic ''Writer


runOutputAsWriter :: Semantic (Output o ': r) a -> Semantic (Writer o ': r) a
runOutputAsWriter = reinterpret \case
  Output o -> tell o
{-# INLINE runOutputAsWriter #-}


runWriter
    :: Monoid o
    => Semantic (Writer o ': r) a
    -> Semantic r (o, a)
runWriter = runState mempty . reinterpretH \case
  Tell o -> do
    modify (<> o) >>= begin
  Listen m -> do
    mm <- start m
    -- TODO(sandy): this is fucking stupid
    (o, fa) <- raise $ runWriter mm
    pure $ fmap (o, ) fa
  Censor f m -> do
    mm <- start m
    ~(o, a) <- raise $ runWriter mm
    modify (<> f o)
    pure a

