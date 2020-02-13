{-# LANGUAGE BangPatterns, TemplateHaskell #-}

module Polysemy.Output
  ( -- * Effect
    Output (..)

    -- * Actions
  , output

    -- * Interpretations
  , runOutputList
  , runLazyOutputList
  , runOutputMonoid
  , runLazyOutputMonoid
  , runOutputMonoidAssocR
  , runLazyOutputMonoidAssocR
  , runOutputMonoidIORef
  , runOutputMonoidTVar
  , outputToIOMonoid
  , outputToIOMonoidAssocR
  , ignoreOutput
  , runOutputBatched
  , runOutputSem
  ) where

import Data.IORef
import Control.Concurrent.STM
import qualified Control.Monad.Trans.Writer.Lazy as Lazy

import Data.Semigroup (Endo(..))
import Data.Bifunctor (first)
import Polysemy
import Polysemy.State
import Control.Monad (when)

import Polysemy.Internal.Union
import Polysemy.Internal.Writer


------------------------------------------------------------------------------
-- | An effect capable of sending messages. Useful for streaming output and for
-- logging.
data Output o m a where
  Output :: o -> Output o m ()

makeSem ''Output


------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a list of its values.
--
-- @since 1.0.0.0
runOutputList
    :: forall o r a
     . Sem (Output o ': r) a
    -> Sem r ([o], a)
runOutputList = fmap (first reverse) . runState [] . reinterpret
  (\case
      Output o -> modify' (o :)
  )
{-# INLINE runOutputList #-}


------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a list of its values,
-- lazily.
--
-- __Warning: This inherits the nasty space leak issue of__
-- __'Lazy.WriterT'! Don't use this if you don't have to.__
--
-- @since 1.3.0.0
runLazyOutputList
    :: forall o r a
     . Sem (Output o ': r) a
    -> Sem r ([o], a)
runLazyOutputList = runLazyOutputMonoidAssocR pure
{-# INLINE runLazyOutputList #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
--
-- @since 1.0.0.0
runOutputMonoid
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runOutputMonoid f = runState mempty . reinterpret
  (\case
      Output o -> modify' (`mappend` f o)
  )
{-# INLINE runOutputMonoid #-}


------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid, and accumulate
-- it lazily.
--
-- __Warning: This inherits the nasty space leak issue of__
-- __'Lazy.WriterT'! Don't use this if you don't have to.__
--
-- @since 1.3.0.0
runLazyOutputMonoid
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runLazyOutputMonoid f = interpretViaLazyWriter $ \(Weaving e s _ ex _) ->
  case e of
    Output o -> ex s <$ Lazy.tell (f o)

------------------------------------------------------------------------------
-- | Like 'runOutputMonoid', but right-associates uses of '<>'.
--
-- This asymptotically improves performance if the time complexity of '<>' for
-- the 'Monoid' depends only on the size of the first argument.
--
-- You should always use this instead of 'runOutputMonoid' if the monoid
-- is a list, such as 'String'.
--
-- @since 1.1.0.0
runOutputMonoidAssocR
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runOutputMonoidAssocR f =
    fmap (first (`appEndo` mempty))
  . runOutputMonoid (\o -> let !o' = f o in Endo (o' <>))
{-# INLINE runOutputMonoidAssocR #-}

------------------------------------------------------------------------------
-- | Like 'runLazyOutputMonoid', but right-associates uses of '<>'.
--
-- This asymptotically improves performance if the time complexity of '<>' for
-- the 'Monoid' depends only on the size of the first argument.
--
-- You should always use this instead of 'runLazyOutputMonoid' if the monoid
-- is a list, such as 'String'.
--
-- __Warning: This inherits the nasty space leak issue of__
-- __'Lazy.WriterT'! Don't use this if you don't have to.__
--
-- @since 1.3.0.0
runLazyOutputMonoidAssocR
    :: forall o m r a
     . Monoid m
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runLazyOutputMonoidAssocR f =
    fmap (first (`appEndo` mempty))
  . runLazyOutputMonoid (\o -> let o' = f o in Endo (o' <>))
                              --   ^ N.B. No bang pattern
{-# INLINE runLazyOutputMonoidAssocR #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into atomic operations
-- over an 'IORef'.
--
-- @since 1.1.0.0
runOutputMonoidIORef
    :: forall o m r a
     . (Monoid m, Member (Embed IO) r)
    => IORef m
    -> (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r a
runOutputMonoidIORef ref f = interpret $ \case
  Output o -> embed $ atomicModifyIORef' ref (\s -> let !o' = f o in (s <> o', ()))
{-# INLINE runOutputMonoidIORef #-}

------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into atomic operations
-- over a 'TVar'.
--
-- @since 1.1.0.0
runOutputMonoidTVar
    :: forall o m r a
     . (Monoid m, Member (Embed IO) r)
    => TVar m
    -> (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r a
runOutputMonoidTVar tvar f = interpret $ \case
  Output o -> embed $ atomically $ do
    s <- readTVar tvar
    writeTVar tvar $! s <> f o
{-# INLINE runOutputMonoidTVar #-}


--------------------------------------------------------------------
-- | Run an 'Output' effect in terms of atomic operations
-- in 'IO'.
--
-- Internally, this simply creates a new 'IORef', passes it to
-- 'runOutputMonoidIORef', and then returns the result and the final value
-- of the 'IORef'.
--
-- /Beware/: As this uses an 'IORef' internally,
-- all other effects will have local
-- state semantics in regards to 'Output' effects
-- interpreted this way.
-- For example, 'Polysemy.Error.throw' and 'Polysemy.Error.catch' will
-- never revert 'output's, even if 'Polysemy.Error.runError' is used
-- after 'outputToIOMonoid'.
--
-- @since 1.2.0.0
outputToIOMonoid
  :: forall o m r a
   . (Monoid m, Member (Embed IO) r)
  => (o -> m)
  -> Sem (Output o ': r) a
  -> Sem r (m, a)
outputToIOMonoid f sem = do
  ref <- embed $ newIORef mempty
  res <- runOutputMonoidIORef ref f sem
  end <- embed $ readIORef ref
  return (end, res)

------------------------------------------------------------------------------
-- | Like 'outputToIOMonoid', but right-associates uses of '<>'.
--
-- This asymptotically improves performance if the time complexity of '<>' for
-- the 'Monoid' depends only on the size of the first argument.
--
-- You should always use this instead of 'outputToIOMonoid' if the monoid
-- is a list, such as 'String'.
--
-- /Beware/: As this uses an 'IORef' internally,
-- all other effects will have local
-- state semantics in regards to 'Output' effects
-- interpreted this way.
-- For example, 'Polysemy.Error.throw' and 'Polysemy.Error.catch' will
-- never revert 'output's, even if 'Polysemy.Error.runError' is used
-- after 'outputToIOMonoidAssocR'.
--
-- @since 1.2.0.0
outputToIOMonoidAssocR
  :: forall o m r a
   . (Monoid m, Member (Embed IO) r)
  => (o -> m)
  -> Sem (Output o ': r) a
  -> Sem r (m, a)
outputToIOMonoidAssocR f =
    (fmap . first) (`appEndo` mempty)
  . outputToIOMonoid (\o -> let !o' = f o in Endo (o' <>))

------------------------------------------------------------------------------
-- | Run an 'Output' effect by ignoring it.
--
-- @since 1.0.0.0
ignoreOutput :: Sem (Output o ': r) a -> Sem r a
ignoreOutput = interpret $ \case
  Output _ -> pure ()
{-# INLINE ignoreOutput #-}


------------------------------------------------------------------------------
-- | Accumulate 'output's so they are delayed until they reach at least size
-- @size@.
--
-- If @size@ is 0, this interpretation will not emit anything in the resulting
-- 'Output' effect.
--
-- @since 1.0.0.0
runOutputBatched
    :: forall o r a
     . Member (Output [o]) r
    => Int
    -> Sem (Output o ': r) a
    -> Sem r a
runOutputBatched 0 m = ignoreOutput m
runOutputBatched size m = do
  ((c, res), a) <-
    runState (0 :: Int, [] :: [o]) $ reinterpret (\case
      Output o -> do
        (count, acc) <- get
        let newCount = 1 + count
            newAcc = o : acc
        if newCount < size
          then put (newCount, newAcc)
          else do
            output (reverse newAcc)
            put (0 :: Int, [] :: [o])
    ) m
  when (c > 0) $ output @[o] (reverse res)
  pure a

------------------------------------------------------------------------------
-- | Runs an 'Output' effect by running a monadic action for each of its
-- values.
runOutputSem :: (o -> Sem r ()) -> Sem (Output o ': r) a -> Sem r a
runOutputSem act = interpret $ \case
    Output o -> act o
{-# INLINE runOutputSem #-}
