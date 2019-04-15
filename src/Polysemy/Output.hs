{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Output
  ( -- * Effect
    Output (..)

    -- * Actions
  , output

    -- * Interpretations
  , runFoldMapOutput
  , runIgnoringOutput
  ) where

import Polysemy
import Polysemy.State


------------------------------------------------------------------------------
-- | An effect capable of sending messages. Useful for streaming output and for
-- logging.
data Output o m a where
  Output :: o -> Output o m ()

makeSem ''Output


------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
runFoldMapOutput
    :: forall o m r a
     . (Typeable m, Monoid m)
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runFoldMapOutput f = runState mempty . reinterpret \case
  Output o -> modify (<> f o)
{-# INLINE runFoldMapOutput #-}


------------------------------------------------------------------------------
-- | Run an 'Ouput' effect by ignoring it.
runIgnoringOutput :: Sem (Output o ': r) a -> Sem r a
runIgnoringOutput = interpret \case
  Output _ -> pure ()
{-# INLINE runIgnoringOutput #-}

