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
  , runOutputAsWriter
  , runWriter
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

makeSemantic ''Writer


------------------------------------------------------------------------------
-- | Transform an 'Output' effect into a 'Writer' effect.
runOutputAsWriter :: Semantic (Output o ': r) a -> Semantic (Writer o ': r) a
runOutputAsWriter = reinterpret \case
  Output o -> tell o
{-# INLINE runOutputAsWriter #-}


------------------------------------------------------------------------------
-- | Run a 'Writer' effect in the style of 'Control.Monad.Trans.Writer.WriterT'
-- (but without the nasty space leak!)
runWriter
    :: (Monoid o, Typeable o)
    => Semantic (Writer o ': r) a
    -> Semantic r (o, a)
runWriter = runState mempty . reinterpretH \case
  Tell o -> do
    modify (<> o) >>= pureT
  Listen m -> do
    mm <- runT m
    -- TODO(sandy): this is fucking stupid
    (o, fa) <- raise $ runWriter mm
    pure $ fmap (o, ) fa
  Censor f m -> do
    mm <- runT m
    ~(o, a) <- raise $ runWriter mm
    modify (<> f o)
    pure a

