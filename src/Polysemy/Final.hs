{-# LANGUAGE TemplateHaskell, PatternGuards #-}

-- | Description: The effect 'Final' that allows embedding higher-order actions in
-- the final target monad of the effect stack
module Polysemy.Final
  (
    -- * Effect
    Final

    -- * Actions
  , withStrategicToFinal
  , controlFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Strategy
    -- | Strategy is a domain-specific language very similar to @Tactics@
    -- (see 'Polysemy.Tactical'), and is used to describe how higher-order
    -- effects are threaded down to the final monad.
    --
    -- Much like @Tactics@, computations can be run and threaded
    -- through the use of 'runS' and 'bindS', and first-order constructors
    -- may use 'pureS'. In addition, 'liftS' may be used to
    -- lift actions of the final monad.
    --
    -- Unlike @Tactics@, the final return value within a 'Strategic'
    -- must be a monadic value of the target monad
    -- with the functorial state wrapped inside of it.
  , Strategic
  , controlS
  , liftWithS
  , restoreS
  , runS
  , controlS'
  , liftWithS'

    -- * Interpretations
  , runM
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal
  ) where

import Polysemy.Internal.Final
