{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Trace where

import Polysemy
import Polysemy.Output


data Trace m a where
  Trace :: String -> Trace m ()

makeSemantic ''Trace


runTraceIO :: Member (Lift IO) r => Semantic (Trace ': r) a -> Semantic r a
runTraceIO = interpret $ \case
  Trace m -> sendM $ putStrLn m
{-# INLINE runTraceIO #-}


runIgnoringTrace :: Member (Lift IO) r => Semantic (Trace ': r) a -> Semantic r a
runIgnoringTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE runIgnoringTrace #-}


runTraceAsOutput :: Semantic (Trace ': r) a -> Semantic (Output String ': r) a
runTraceAsOutput = reinterpret $ \case
  Trace m -> output m
{-# INLINE runTraceAsOutput #-}

