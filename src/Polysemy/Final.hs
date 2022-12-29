{-# LANGUAGE TemplateHaskell, PatternGuards #-}

-- | Description: The effect 'Final' that allows embedding higher-order actions in
-- the final target monad of the effect stack
module Polysemy.Final
  (
    -- * Effect
    Final

    -- * Actions
  , controlFinal
  , withStrategicToFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Strategy
    -- | 'Strategic' is a domain-specific language very similar in
    -- use to 'Polysemy.Interpretation.Handling', and is used to describe how
    -- higher-order effects are threaded down to the final monad.
    --
    -- 'withStrategicToFinal' should be used when 'controlFinal' is
    -- not powerful enough for your purposes. Notable combinators include
    -- 'runS', 'embed', 'liftWithS', and 'restoreS'.
  , Strategic

    -- ** Lifting the final monad to 'Strategic'
  , embed

    -- ** Lowering computations to the final monad
  , withProcessorS
  , controlWithProcessorS
  , processS

    -- ** Embedding computations into 'Strategic'
  , runS

    -- ** Lowering 'Strategic' to the final monad
  , controlS
  , liftWithS

    -- ** Manipulating effectful state
  , restoreS
  , runExposeS
  , exposeS

    -- * Interpretations
  , runM
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal
  ) where

import Polysemy.Internal.Final
