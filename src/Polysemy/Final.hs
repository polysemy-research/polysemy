{-# LANGUAGE TemplateHaskell, PatternGuards #-}

-- | Description: The effect 'Final' that allows embedding higher-order actions in
-- the final target monad of the effect stack
module Polysemy.Final
  (
    -- * Effect
    Final

    -- * Actions
  , controlFinal
  , withLoweringToFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Lowering
    -- | 'Lowering' is a domain-specific language very similar in
    -- use to 'Polysemy.HigherOrder', and is used to describe how
    -- higher-order effects are threaded down to the final monad.
    --
    -- 'withLoweringToFinal' should be used when 'controlFinal' is
    -- not powerful enough for your purposes. Notable combinators include
    -- 'runL', 'embed', 'liftWithL', and 'restoreL'.
  , Lowering

    -- ** Lifting the final monad to 'Lowering'
  , embed

    -- ** Lowering computations to the final monad
  , withProcessorL
  , controlWithProcessorL
  , processL

    -- ** Embedding computations into 'Lowering'
  , runL

    -- ** Lowering 'Lowering' to the final monad
  , controlL
  , liftWithL

    -- ** Manipulating effectful state
  , restoreL
  , runExposeL
  , exposeL

    -- * Interpretations
  , runM
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal
  ) where

import Polysemy.Internal.Final
