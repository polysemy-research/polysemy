{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

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


-- -- inlineRecursiveCalls [d|
-- runWriter
--     :: Monoid o
--     => Semantic (Writer o ': r) a
--     -> Semantic r (o, a)
-- runWriter = runState mempty . reinterpretH \case
--   Tell o -> do
--     start $ modify (<> o)
--   -- Listen m -> do
--   --   raise $ runWriter m
--   -- Censor f m -> do
--   --   ~(o, a) <- raise $ runWriter m
--   --   modify (<> f o)
--   --   pure a
-- -- |]

