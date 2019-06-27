{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Polysemy.Writer
  ( -- * Effect
    Writer (..)

    -- * Actions
  , tell
  , listen
  , censor

    -- * Interpretations
  , runWriter

    -- * Interpretations for Other Effects
  , runOutputAsWriter
  ) where

import Polysemy
import Polysemy.Output
import Polysemy.State

------------------------------------------------------------------------------
-- | An effect capable of emitting and intercepting messages.
data Writer o m a where
  Tell   :: o -> Writer o m ()
  Listen :: ∀ o m a. m a -> Writer o m (o, a)
  Censor :: (o -> o) -> m a -> Writer o m a

makeSem ''Writer


------------------------------------------------------------------------------
-- | Transform an 'Output' effect into a 'Writer' effect.
runOutputAsWriter :: Member (Writer o) r => Sem (Output o ': r) a -> Sem r a
runOutputAsWriter = interpret \case
  Output o -> tell o
{-# INLINE runOutputAsWriter #-}


------------------------------------------------------------------------------
-- | Run a 'Writer' effect in the style of 'Control.Monad.Trans.Writer.WriterT'
-- (but without the nasty space leak!)
runWriter
    :: Monoid o
    => Sem (Writer o ': r) a
    -> Sem r (o, a)
runWriter = runState mempty . reinterpretH \case
  Tell o -> do
    modify (<> o) >>= pureT
  Listen m -> do
    mm <- runT m
    -- TODO(sandy): this is stupid
    (o, fa) <- raise $ runWriter mm
    pure $ fmap (o, ) fa
  Censor f m -> do
    mm <- runT m
    ~(o, a) <- raise $ runWriter mm
    modify (<> f o)
    pure a

