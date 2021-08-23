-- | Tools for more advanced usages of 'Polysemy.interpretNew'
module Polysemy.Interpretation
  ( -- * Manipuluating effectful state
    runExposeH
  , runExposeH'
  , exposeH
  , restoreH
  , propagate

  -- * Lowering Higher-Order thunks to actions of @'Sem' r@.
  , Processor(..)
  , liftWithH
  , controlH
  , getProcessorH
  ) where

import Polysemy.Internal.Interpretation
