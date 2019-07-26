{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Fixpoint
  ( -- * Effect
    Fixpoint (..)

    -- * Interpretations
  , module Polysemy.Fixpoint
  ) where

import Control.Monad.Fix
import Polysemy
import Polysemy.Internal.Fixpoint

------------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect purely.
--
-- __Note__: 'runFixpoint' operates under the assumption that any effectful
-- state which can't be inspected using 'Polysemy.Inspector' can't contain any
-- values. This is true for all interpreters featured in this package,
-- and is presumably always true for any properly implemented interpreter.
--
-- If 'runFixpoint' throws an exception for you, then you're likely using
-- some interpreter outside of this package that uses
-- 'Polysemy.Internal.Union.weave' improperly.
-- If this cannot possibly be the case, open an issue over at the
-- GitHub repository.
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
        maybe (bomb "runFixpoint") id (inspect ins fa) <$ s
{-# INLINE runFixpoint #-}

------------------------------------------------------------------------------
-- | Run a 'Fixpoint' effect in terms of an underlying 'MonadFix' instance.
--
-- __Note__: 'runFixpointM' operates under the assumption that any effectful
-- state which can't be inspected using 'Polysemy.Inspector' can't contain any
-- values. This is true for all interpreters featured in this package,
-- and is presumably always true for any properly implemented interpreter.
--
-- If 'runFixpointM' throws an exception for you, then you're likely using
-- some interpreter outside of this package that uses
-- 'Polysemy.Internal.Union.weave' improperly.
-- If this cannot possibly be the case, open an issue over at the
-- GitHub repository.
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
        maybe (bomb "runFixpointM") id (inspect ins fa) <$ s
{-# INLINE runFixpointM #-}


bomb :: String -> a
bomb str = error $
    str ++ ": Uninspectable effectful state still\
            \ contained an observable result.\
            \ You're likely using an interpreter\
            \ that uses 'weave' improperly.\
            \ See documentation for more information."
