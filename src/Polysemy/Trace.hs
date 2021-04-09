{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Trace
  ( -- * Effect
    Trace (..)

    -- * Actions
  , trace

    -- * Interpretations
  , traceToHandle
  , traceToStdout
  , traceToStderr
  , traceToIO
  , runTraceList
  , ignoreTrace
  , traceToOutput

    -- * Interpretations for Other Effects
  , outputToTrace
  ) where

import Polysemy
import Polysemy.Output
import System.IO (stdout, stderr, hPutStrLn, Handle)


------------------------------------------------------------------------------
-- | An effect for logging strings.
data Trace m a where
  Trace :: String -> Trace m ()

makeSem ''Trace


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to the provided 'Handle'.
--
-- @since TODO
traceToHandle :: Member (Embed IO) r => Handle -> Sem (Trace ': r) a -> Sem r a
traceToHandle handle = interpret $ \case
  Trace m -> embed $ hPutStrLn handle m
{-# INLINE traceToHandle #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to stdout.
--
-- @since TODO
traceToStdout :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToStdout = traceToHandle stdout
{-# INLINE traceToStdout #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to stderr.
--
-- @since TODO
traceToStderr :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToStderr = traceToHandle stderr
{-# INLINE traceToStderr #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to stdout.
--
-- @since 1.0.0.0
traceToIO :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToIO = traceToStdout
{-# INLINE traceToIO #-}
{-# deprecated traceToIO "Use traceToStdout" #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
--
-- @since 1.0.0.0
ignoreTrace :: Sem (Trace ': r) a -> Sem r a
ignoreTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}


------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
--
-- @since 1.0.0.0
traceToOutput
    :: Member (Output String) r
    => Sem (Trace ': r) a
    -> Sem r a
traceToOutput = interpret $ \case
  Trace m -> output m
{-# INLINE traceToOutput #-}


------------------------------------------------------------------------------
-- | Get the result of a 'Trace' effect as a list of 'String's.
--
-- @since 1.0.0.0
runTraceList
    :: Sem (Trace ': r) a
    -> Sem r ([String], a)
runTraceList = runOutputList . reinterpret (
  \case
    Trace m -> output m
  )
{-# INLINE runTraceList #-}


------------------------------------------------------------------------------
-- | Transform an @'Output' w@ effect into a 'Trace' effect given a function
-- to transform each @w@ to a 'String'.
--
-- @since 1.0.0.0
outputToTrace
  :: forall w r a
   . Member Trace r
  => (w -> String)
  -> Sem (Output w ': r) a
  -> Sem r a
outputToTrace show' = interpret $ \case
  Output m -> trace $ show' m
{-# INLINE outputToTrace #-}

