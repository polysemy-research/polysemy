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
  , runWriterIORef
  , runWriterTVar

    -- * Interpretations for Other Effects
  , outputToWriter
  ) where

import Control.Concurrent.STM

import Data.Bifunctor (first)
import Data.IORef

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

------------------------------------------------------------------------------
-- | Run a 'Writer' effect by transforming it into atomic
-- operations over an 'IORef'.
--
-- /Note/: All 'tell's in the argument of a 'listen' or 'pass' will temporarily
-- be stored seperately from the provided 'IORef' until the 'listen'\/'pass'
-- completes.
-- Therefore, such 'tell's will be lost if the argument to the 'listen'\/'pass'
-- fails due to any effect interpreted after 'runWriterIORef', or due to an
-- 'IO' exception.
--
-- In addition, any 'tell's of any asynchronous computation spawned inside a
-- 'listen'\/'pass' will be lost once the 'listen'\/'pass' completes.
-- 'runWriterTVar' avoids this particular issue.
runWriterIORef
  :: (Monoid o, Member (Embed IO) r)
  => IORef o
  -> Sem (Writer o ': r) a
  -> Sem r a
runWriterIORef ref = interpretH $ \case
  Tell o -> do
    t <- embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
    pureT t
  Listen m -> do
    mm   <- runT m
    -- TODO(KingoftheHomeless): this is REALLY stupid
    ref' <- embed $ newIORef mempty
    fa   <- raise $ runWriterIORef ref' mm
    o    <- embed $ readIORef ref'
    embed $ atomicModifyIORef' ref (\s -> (s <> o, ()))
    return $ fmap (o, ) fa
  Pass m -> do
    mm   <- runT m
    ins  <- getInspectorT
    ref' <- embed $ newIORef mempty
    t    <- raise $ runWriterIORef ref' mm
    let f = maybe id fst $ inspect ins t
    o    <- embed $ readIORef ref'
    embed $ atomicModifyIORef' ref (\s -> (s <> f o, ()))
    return (fmap snd t)

------------------------------------------------------------------------------
-- | Run a 'Writer' effect by transforming it into atomic operations over a
-- 'TVar'.
--
-- /Note/: All 'tell's in the argument of a 'listen' or 'pass' will temporarily
-- be stored seperately from the provided 'TVar' until the 'listen'\/'pass'
-- completes.
-- Therefore, such 'tell's will be lost if the argument to the 'listen'\/'pass'
-- fails due to any effect interpreted after 'runWriterIORef',
-- or due to an 'IO' exception.
runWriterTVar :: (Monoid o, Member (Embed IO) r)
              => TVar o
              -> Sem (Writer o ': r) a
              -> Sem r a
runWriterTVar tvar = runWriterTVarAction $ \o -> do
  s <- readTVar tvar
  writeTVar tvar $! s <> o
{-# INLINE runWriterTVar #-}

runWriterTVarAction :: forall o a r
                     . (Monoid o, Member (Embed IO) r)
                    => (o -> STM ())
                    -> Sem (Writer o ': r) a
                    -> Sem r a
runWriterTVarAction act = interpretH $ \case
  Tell o -> do
    t <- embed $ atomically (act o)
    pureT t
  Listen m -> do
    mm     <- runT m
    {-
       KingoftheHomeless: this switch controls how 'tell's of
       'mm' are committed. As long as 'mm' is running,
       it and any async computations spawned by it will write to
       the local tvar. Once the 'listen' has
       completed, the switch will flip, and any async computations
       spawned by 'mm' will write to the
       global tvar (which is represented by 'act').

       Since the switch flip is part of the same 'atomically' that
       moves the writes of the local tvar to the global tvar,
       no writes of 'mm' are lost (unless 'mm' fails).
    -}
    switch <- embed $ newTVarIO False
    tvar   <- embed $ newTVarIO mempty
    fa     <- raise $ runWriterTVarAction (act' tvar switch) mm
    embed $ atomically $ do
      o <- readTVar tvar
      act o
      writeTVar switch True
      return (fmap (o, ) fa)
  Pass m -> do
    mm     <- runT m
    ins    <- getInspectorT
    switch <- embed $ newTVarIO False
    tvar   <- embed $ newTVarIO mempty
    t      <- raise $ runWriterTVarAction (act' tvar switch) mm
    embed $ atomically $ do
      let f = maybe id fst (inspect ins t)
      o <- readTVar tvar
      act $! f o
      writeTVar switch True
      return (fmap snd t)

  where
    act' :: TVar o
         -> TVar Bool
         -> o
         -> STM ()
    act' tvar switch = \o -> do
      useGlobal <- readTVar switch
      if useGlobal then
        act o
      else do
        r <- readTVar tvar
        writeTVar tvar $! r <> o
{-# INLINE runWriterTVarAction #-}
