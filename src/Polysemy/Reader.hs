{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Reader
  ( -- * Effect
    Reader (..)

    -- * Actions
  , ask
  , asks
  , local

    -- * Interpretations
  , runReader

    -- * Interpretations for Other Effects
  , inputToReader
  ) where

import Polysemy
import Polysemy.Input


------------------------------------------------------------------------------
-- | An effect corresponding to 'Control.Monad.Trans.Reader.ReaderT'.
data Reader i m a where
  Ask   :: Reader i m i
  Local :: (i -> i) -> m a -> Reader i m a

makeSem ''Reader


asks :: forall i j r. Member (Reader i) r => (i -> j) -> Sem r j
asks f = f <$> ask
{-# INLINABLE asks #-}


------------------------------------------------------------------------------
-- | Run a 'Reader' effect with a constant value.
runReader :: i -> Sem (Reader i ': r) a -> Sem r a
runReader i = interpretH $ \case
  Ask -> pureT i
  Local f m -> do
    mm <- runT m
    raise $ runReader (f i) mm
{-# INLINE runReader #-}


------------------------------------------------------------------------------
-- | Transform an 'Input' effect into a 'Reader' effect.
--
-- @since 1.0.0.0
inputToReader :: Member (Reader i) r => Sem (Input i ': r) a -> Sem r a
inputToReader = interpret $ \case
  Input -> ask
{-# INLINE inputToReader #-}

