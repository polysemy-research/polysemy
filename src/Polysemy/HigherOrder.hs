-- | Tools for more advanced usages of 'Polysemy.interpretH'
module Polysemy.HigherOrder
  ( -- * 'HigherOrder' effect
    HigherOrder
  , higherOrderIntoOpaque
  , higherOrderFromOpaque

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

import Polysemy
import Polysemy.Opaque
import Polysemy.Internal.HigherOrder
import Polysemy.Internal.Utils

higherOrderIntoOpaque :: forall e z t eH rH r x
                       . Sem (e ': HigherOrder z t eH rH ': r) x
                      -> Sem (e ': Opaque (HigherOrder z t eH rH) ': r) x
higherOrderIntoOpaque = coerceEffs

higherOrderFromOpaque :: forall e z t eH rH r x
                       . Sem (e ': Opaque (HigherOrder z t eH rH) ': r) x
                      -> Sem (e ': HigherOrder z t eH rH ': r) x
higherOrderFromOpaque = coerceEffs
