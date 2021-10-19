{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Async
  ( -- * Effect
    Async (..)

    -- * Actions
  , async
  , await
  , cancel

    -- * Helpers
  , sequenceConcurrently

    -- * Interpretations
  , asyncToIOFinal
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
  Cancel :: A.Async a -> Async m ()

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
asyncToIOFinal = interpretFinal @IO $ \case
  Async m -> liftWithS $ \lower -> do
    fmap (foldr (const . Just) Nothing) <$> A.async (lower m)
  Await a -> embed (A.wait a)
  Cancel a -> embed (A.cancel a)
{-# INLINE asyncToIOFinal #-}

