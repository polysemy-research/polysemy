{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Trace where

import Polysemy
import Polysemy.Effect.New
import Polysemy.Output


data Trace m a = Trace String a
  deriving (Functor, Effect)

makeSemantic ''Trace


runTraceIO :: Member (Lift IO) r => Semantic (Trace ': r) a -> Semantic r a
runTraceIO = interpret $ \case
  Trace m k -> sendM (putStrLn m) >> pure k
{-# INLINE runTraceIO #-}


runTraceAsOutput :: Semantic (Trace ': r) a -> Semantic (Output String ': r) a
runTraceAsOutput = reinterpret $ \case
  Trace m k -> output m >> pure k
{-# INLINE runTraceAsOutput #-}

