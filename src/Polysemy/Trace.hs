{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Trace
  ( -- * Effect
    Trace (..)

    -- * Actions
  , trace

    -- * Interpretations
  , runTraceIO
  , runIgnoringTrace
  , runTraceAsOutput

    -- * Interpretations for Other Effects
  , runOutputAsTrace
  ) where

import Polysemy
import Polysemy.Output


------------------------------------------------------------------------------
-- | An effect for logging strings.
data Trace m a where
  Trace :: String -> Trace m ()

makeSem ''Trace


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to stdout.
runTraceIO :: Member (Lift IO) r => Sem (Trace ': r) a -> Sem r a
runTraceIO = interpret $ \case
  Trace m -> sendM $ putStrLn m
{-# INLINE runTraceIO #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
runIgnoringTrace :: Member (Lift IO) r => Sem (Trace ': r) a -> Sem r a
runIgnoringTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE runIgnoringTrace #-}


------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
runTraceAsOutput
    :: Member (Output String) r
    => Sem (Trace ': r) a
    -> Sem r a
runTraceAsOutput = interpret $ \case
  Trace m -> output m
{-# INLINE runTraceAsOutput #-}

------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
--
-- @since 0.1.2.0
runOutputAsTrace
    :: ( Show w
       , Member Trace r
       )
    => Sem (Output w ': r) a
    -> Sem r a
runOutputAsTrace = interpret $ \case
  Output m -> trace $ show m
{-# INLINE runOutputAsTrace #-}

