-- | Tools for more advanced usages of 'Polysemy.interpretH'
module Polysemy.HigherOrder
  ( -- * 'HigherOrder' effect
    HigherOrder

    -- * Ultrageneric 'interpretH'
  , genericInterpretH

    -- * Running higher-order chunks
  , runH
  , runH'

    -- * Propagating actions
  , propagate
  , propagateUsing

    -- * Processing higher-order chunks
  , withProcessorH
  , controlWithProcessorH
  , processH

    -- * Manipulating effectful state
  , restoreH
  , runExposeH
  , runExposeH'
  , exposeH

    -- * Retrieving the current interpreter
  , InterpreterH(..)
  , getInterpreterH

  -- * Lowering @'Polysemy.Sem' ('HigherOrder' ... ': r)@ to @'Polysemy.Sem' r@
  , liftWithH
  , controlH

  -- * Retrieving the type parameters of a 'HigherOrder'
  , TypeParamsH(..)
  , getTypeParamsH
  ) where

import Polysemy.Internal.HigherOrder
