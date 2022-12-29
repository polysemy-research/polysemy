-- | Tools for more advanced usages of 'Polysemy.interpretH'
module Polysemy.Interpretation
  ( -- * Manipuluating effectful state
    runExposeH
  , runExposeH'
  , exposeH
  , restoreH
  , propagate
  , propagateUsing

  -- * Lowering Higher-Order thunks to actions of @'Sem' r@.
  , liftWithH
  , controlH
  , ProcessorH(..)
  , getProcessorH
  , InterpreterH(..)
  , getInterpreterH
  ) where

import Polysemy.Internal.Interpretation
