{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Async
  ( -- * Effect
    Async (..)

    -- * Actions
  , async
  , await

    -- * Helpers
  , sequenceConcurrently

    -- * Interpretations
  , asyncToIO
  , asyncToIOFinal
  , lowerAsync
  ) where

import qualified Control.Concurrent.Async as A
import           Polysemy
import           Polysemy.Final



------------------------------------------------------------------------------
-- | An effect for spawning asynchronous computations.
--
-- The 'Maybe' returned by 'async' is due to the fact that we can't be sure an
-- 'Polysemy.Error.Error' effect didn't fail locally.
--
-- @since 0.5.0.0
data Async m a where
  Async :: m a -> Async m (A.Async (Maybe a))
  Await :: A.Async a -> Async m a

makeSem ''Async


------------------------------------------------------------------------------
-- | Perform a sequence of effectful actions concurrently.
--
-- @since 1.2.2.0
sequenceConcurrently :: forall t r a. (Traversable t, Member Async r) =>
    t (Sem r a) -> Sem r (t (Maybe a))
sequenceConcurrently t = traverse async t >>= traverse await
{-# INLINABLE sequenceConcurrently #-}


------------------------------------------------------------------------------
-- | A more flexible --- though less performant ---
-- version of 'asyncToIOFinal'.
--
-- This function is capable of running 'Async' effects anywhere within an
-- effect stack, without relying on 'Final' to lower it into 'IO'.
-- Notably, this means that 'Polysemy.State.State' effects will be consistent
-- in the presence of 'Async'.
--
-- 'asyncToIO' is __unsafe__ if you're using 'await' inside higher-order actions
-- of other effects interpreted after 'Async'.
-- See <https://github.com/polysemy-research/polysemy/issues/205 Issue #205>.
--
-- Prefer 'asyncToIOFinal' unless you need to run pure, stateful interpreters
-- after the interpreter for 'Async'.
-- (Pure interpreters are interpreters that aren't expressed in terms of
-- another effect or monad; for example, 'Polysemy.State.runState'.)
--
-- @since 1.0.0.0
asyncToIO
    :: Member (Embed IO) r
    => Sem (Async ': r) a
    -> Sem r a
asyncToIO m = withLowerToIO $ \lower _ -> lower $
  interpretH
    ( \case
        Async a -> do
          ma  <- runT a
          ins <- getInspectorT
          fa  <- embed $ A.async $ lower $ asyncToIO ma
          pureT $ fmap (inspect ins) fa

        Await a -> pureT =<< embed (A.wait a)
    )  m
{-# INLINE asyncToIO #-}

------------------------------------------------------------------------------
-- | Run an 'Async' effect in terms of 'A.async' through final 'IO'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Async' effects
-- interpreted this way. See 'Final'.
--
-- Notably, unlike 'asyncToIO', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateIORef' is used.
-- State that seems like it should be threaded globally throughout 'Async'
-- /will not be./
--
-- Use 'asyncToIO' instead if you need to run
-- pure, stateful interpreters after the interpreter for 'Async'.
-- (Pure interpreters are interpreters that aren't expressed in terms of
-- another effect or monad; for example, 'Polysemy.State.runState'.)
--
-- @since 1.2.0.0
asyncToIOFinal :: Member (Final IO) r
               => Sem (Async ': r) a
               -> Sem r a
asyncToIOFinal = interpretFinal $ \case
  Async m -> do
    ins <- getInspectorS
    m'  <- runS m
    liftS $ A.async (inspect ins <$> m')
  Await a -> liftS (A.wait a)
{-# INLINE asyncToIOFinal #-}

------------------------------------------------------------------------------
-- | Run an 'Async' effect in terms of 'A.async'.
--
-- @since 1.0.0.0
lowerAsync
    :: Member (Embed IO) r
    => (forall x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is likely
       -- some combination of 'runM' and other interpreters composed via '.@'.
    -> Sem (Async ': r) a
    -> Sem r a
lowerAsync lower m = interpretH
    ( \case
        Async a -> do
          ma  <- runT a
          ins <- getInspectorT
          fa  <- embed $ A.async $ lower $ lowerAsync lower ma
          pureT $ fmap (inspect ins) fa

        Await a -> pureT =<< embed (A.wait a)
    )  m
{-# INLINE lowerAsync #-}
{-# DEPRECATED lowerAsync "Use 'asyncToIOFinal' instead" #-}
