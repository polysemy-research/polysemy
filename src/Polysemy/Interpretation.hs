-- | Tools for more advanced usages of 'Polysemy.interpretNew'
module Polysemy.Interpretation
  ( -- * Manipuluating effectful state
    runExposeH
  , runExposeH'
  , exposeH
  , restoreH

  -- * Lowering Higher-Order thunks to actions of @'Sem' r@.
  , Processor(..)
  , getProcessorH
  , getProcessorH'
  ) where

import Polysemy.Internal.Interpretation
