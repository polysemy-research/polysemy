{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Async
  ( -- * Effect
    Async (..)

    -- * Actions
  , async
  , await

    -- * Interpretations
  , runAsync
  ) where

import qualified Control.Concurrent.Async as A
import           Polysemy
import           Polysemy.Internal.Forklift



------------------------------------------------------------------------------
-- |
--
-- TODO(sandy): @since
data Async m a where
  Async :: m a -> Async m (A.Async (Maybe a))
  Await :: A.Async a -> Async m a

makeSem ''Async


------------------------------------------------------------------------------
-- |
--
-- TODO(sandy): @since
runAsync
    :: LastMember (Lift IO) r
    => Sem (Async ': r) a
    -> Sem r a
runAsync m = withLowerToIO $ \lower _ -> lower $
  interpretH
    ( \case
        Async a -> do
          ma  <- runT a
          ins <- getInspectorT
          fa  <- sendM $ A.async $ lower $ runAsync_b $ ma
          pureT $ fmap (inspect ins) fa

        Await a -> pureT =<< sendM (A.wait a)
    )  m
{-# INLINE runAsync #-}


runAsync_b
    :: LastMember (Lift IO) r
    => Sem (Async ': r) a
    -> Sem r a
runAsync_b = runAsync
{-# NOINLINE runAsync_b #-}

