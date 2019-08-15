{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Polysemy.Writer
  ( -- * Effect
    Writer (..)

    -- * Actions
  , tell
  , listen
  , pass
  , censor

    -- * Interpretations
  , runWriter
  , runWriterAssocR

    -- * Interpretations for Other Effects
  , outputToWriter
  ) where

import Data.Bifunctor (first)

import Polysemy
import Polysemy.Output
import Polysemy.State


------------------------------------------------------------------------------
-- | An effect capable of emitting and intercepting messages.
data Writer o m a where
  Tell   :: o -> Writer o m ()
  Listen :: ∀ o m a. m a -> Writer o m (o, a)
  Pass   :: m (o -> o, a) -> Writer o m a

makeSem ''Writer

------------------------------------------------------------------------------
-- | @since 0.7.0.0
censor :: Member (Writer o) r
       => (o -> o)
       -> Sem r a
       -> Sem r a
censor f m = pass (fmap (f ,) m)
{-# INLINE censor #-}

------------------------------------------------------------------------------
-- | Transform an 'Output' effect into a 'Writer' effect.
--
-- @since 1.0.0.0
outputToWriter :: Member (Writer o) r => Sem (Output o ': r) a -> Sem r a
outputToWriter = interpret $ \case
  Output o -> tell o
{-# INLINE outputToWriter #-}


------------------------------------------------------------------------------
-- | Run a 'Writer' effect in the style of 'Control.Monad.Trans.Writer.WriterT'
-- (but without the nasty space leak!)
runWriter
    :: Monoid o
    => Sem (Writer o ': r) a
    -> Sem r (o, a)
runWriter = runState mempty . reinterpretH
  (\case
      Tell o -> do
        modify' (<> o) >>= pureT
      Listen m -> do
        mm <- runT m
        -- TODO(sandy): this is stupid
        (o, fa) <- raise $ runWriter mm
        modify' (<> o)
        pure $ fmap (o, ) fa
      Pass m -> do
        mm <- runT m
        (o, t) <- raise $ runWriter mm
        ins <- getInspectorT
        let f = maybe id fst (inspect ins t)
        modify' (<> f o)
        pure (fmap snd t)
  )
{-# INLINE runWriter #-}

-----------------------------------------------------------------------------
-- | Like 'runWriter', but right-associates uses of '<>'.
--
-- This asymptotically improves performance if the time complexity of '<>'
-- for the 'Monoid' depends only on the size of the first argument.
--
-- You should always use this instead of 'runWriter' if the monoid
-- is a list, such as 'String'.
runWriterAssocR
    :: Monoid o
    => Sem (Writer o ': r) a
    -> Sem r (o, a)
runWriterAssocR =
  let
    go :: forall o r a
        . Monoid o
       => Sem (Writer o ': r) a
       -> Sem r (o -> o, a)
    go =
        runState id
      . reinterpretH
      (\case
          Tell o -> do
            modify' @(o -> o) (. (o <>)) >>= pureT
          Listen m -> do
            mm <- runT m
            -- TODO(sandy): this is stupid
            (oo, fa) <- raise $ go mm
            modify' @(o -> o) (. oo)
            pure $ fmap (oo mempty, ) fa
          Pass m -> do
            mm <- runT m
            (o, t) <- raise $ runWriterAssocR mm
            ins <- getInspectorT
            let f = maybe id fst (inspect ins t)
            modify' @(o -> o) (. (f o <>))
            pure (fmap snd t)
      )
    {-# INLINE go #-}
  in fmap (first ($ mempty)) . go
{-# INLINE runWriterAssocR #-}
