{-# LANGUAGE AllowAmbiguousTypes, TemplateHaskell #-}

-- | Description: Interpreters for 'Fixpoint'
module Polysemy.Fixpoint
  ( -- * Effect
    Fixpoint (..)

    -- * Interpretations
  , module Polysemy.Fixpoint
  ) where

import Control.Monad.Fix

import Polysemy
import Polysemy.Final
import Polysemy.Internal.Fixpoint

-----------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect in terms of a final 'MonadFix' instance.
--
-- If you need to run a 'Fixpoint' effect purely, use this together with
-- @'Final' 'Data.Functor.Identity.Identity'@.
--
-- __Note__: This is subject to the same traps as 'MonadFix' instances for
-- monads with failure: this will throw an exception if you try to recursively use
-- the result of a failed computation in an action whose effect may be observed
-- even though the computation failed.
--
-- For example, the following program will throw an exception upon evaluating the
-- final state:
--
-- @
-- bad :: (Int, Either () Int)
-- bad =
--    'Data.Functor.Identity.runIdentity'
--  . 'runFinal'
--  . 'fixpointToFinal' \@'Data.Functor.Identity.Identity'
--  . 'Polysemy.State.runLazyState' \@Int 1
--  . 'Polysemy.Error.runError'
--  $ mdo
--   'Polysemy.State.put' a
--   a <- 'Polysemy.Error.throw' ()
--   return a
-- @
--
-- 'fixpointToFinal' also operates under the assumption that any effectful
-- state which can't be inspected using 'Polysemy.Inspector' can't contain any
-- values. For example, the effectful state for 'Polysemy.Error.runError' is
-- @'Either' e a@. The inspector for this effectful state only fails if the
-- effectful state is a @'Left'@ value, which therefore doesn't contain any
-- values of @a@.
--
-- This assumption holds true for all interpreters featured in this package,
-- and is presumably always true for any properly implemented interpreter.
-- 'fixpointToFinal' may throw an exception if it is used together with an
-- interpreter that uses 'Polysemy.Internal.Union.weave' improperly.
--
-- If 'fixpointToFinal' throws an exception for you, and it can't
-- be due to any of the above, then open an issue over at the
-- GitHub repository for polysemy.
--
-- @since 1.2.0.0
fixpointToFinal :: forall m r a
                 . (Member (Final m) r, MonadFix m)
                => Sem (Fixpoint ': r) a
                -> Sem r a
fixpointToFinal = interpretFinal @m $ \case
  Fixpoint f -> controlWithProcessorL $ \lower ->
    mfix $ lower . f . foldr const (bomb "fixpointToFinal")
{-# INLINE fixpointToFinal #-}
