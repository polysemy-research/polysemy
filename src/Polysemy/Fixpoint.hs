{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Fixpoint
  ( -- * Effect
    Fixpoint (..)

    -- * Interpretations
  , module Polysemy.Fixpoint
  ) where

import Control.Monad.Fix
import Data.Maybe

import Polysemy
import Polysemy.Internal.Fixpoint

------------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect purely.
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
--    'run'
--  . 'runFixpoint' 'run'
--  . 'Polysemy.State.runLazyState' @Int 1
--  . 'Polysemy.Error.runError'
--  $ mdo
--   'Polysemy.State.put' a
--   a <- 'Polysemy.Error.throw' ()
--   return a
-- @
--
-- 'runFixpoint' also operates under the assumption that any effectful
-- state which can't be inspected using 'Polysemy.Inspector' can't contain any
-- values. This is true for all interpreters featured in this package,
-- and is presumably always true for any properly implemented interpreter.
-- 'runFixpoint' may throw an exception if it is used together with an
-- interpreter that uses 'Polysemy.Internal.Union.weave' improperly.
--
-- If 'runFixpoint' throws an exception for you, and it can't
-- be due to any of the above, then open an issue over at the
-- GitHub repository for polysemy.
runFixpoint
    :: (∀ x. Sem r x -> x)
    -> Sem (Fixpoint ': r) a
    -> Sem r a
runFixpoint lower = interpretH $ \case
  Fixpoint mf -> do
    c   <- bindT mf
    s   <- getInitialStateT
    ins <- getInspectorT
    pure $ fix $ \fa ->
      lower . runFixpoint lower . c $
        fromMaybe (bomb "runFixpoint") (inspect ins fa) <$ s
{-# INLINE runFixpoint #-}

------------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect in terms of an underlying 'MonadFix' instance.
--
-- __Note__: 'runFixpointM' is subject to the same caveats as 'runFixpoint'.
runFixpointM
    :: ( MonadFix m
       , Member (Embed m) r
       )
    => (∀ x. Sem r x -> m x)
    -> Sem (Fixpoint ': r) a
    -> Sem r a
runFixpointM lower = interpretH $ \case
  Fixpoint mf -> do
    c   <- bindT mf
    s   <- getInitialStateT
    ins <- getInspectorT
    embed $ mfix $ \fa ->
      lower . runFixpointM lower . c $
        fromMaybe (bomb "runFixpointM") (inspect ins fa) <$ s
{-# INLINE runFixpointM #-}
