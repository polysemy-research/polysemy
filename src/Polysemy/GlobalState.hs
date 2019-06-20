{-# LANGUAGE TemplateHaskell #-}

module Polysemy.GlobalState
  ( -- * Effect
    GlobalState (..)

    -- * Actions
  , allocate
  , getGlobal
  , putGlobal
  , modifyGlobal

    -- * Interpretations
  , runGlobalState
  , runGlobalStateInIO

    -- * Interpretations for Other Effects
  , runStateAsGlobal
  ) where

import Data.AnyStore
import Data.IORef
import GHC.Exts
import Polysemy
import Polysemy.State
import System.IO.Unsafe



data GlobalState m a where
  Allocate :: a -> (forall p. Key p a -> m b) -> GlobalState m b
  GetGlobal :: Key p a -> GlobalState m a
  PutGlobal :: Key p a -> a -> GlobalState m ()

makeSem ''GlobalState


modifyGlobal :: Member GlobalState r => (forall p. Key p s) -> (s -> s) -> Sem r ()
modifyGlobal k f = getGlobal k >>= putGlobal k . f

runStateAsGlobal
    :: forall s r a
     . Member GlobalState r
    => s
    -> Sem (State s ': r) a
    -> Sem r a
runStateAsGlobal s m = allocate s $ \key ->
  interpret
    ( \case
        Get    -> getGlobal key
        Put s' -> putGlobal key s'
    ) m


runGlobalState :: Sem (GlobalState ': r) a -> Sem r a
runGlobalState = evalState @(AnyStore Any) emptyS . reinterpretH (\case
  Allocate a f -> do
    (store', key) <- gets $ allocS @Any a
    key' <- pureT key
    put store'
    f' <- bindT f
    r <- raise $ runGlobalState $ f' key'
    modify' $ freeS key
    pure r
  GetGlobal key -> pureT =<< gets (getS @Any $ fiddleKey key)
  PutGlobal key a -> do
    modify' $ putS @Any (fiddleKey key) a
    getInitialStateT
  )


runGlobalStateInIO :: Member (Lift IO) r => Sem (GlobalState ': r) a -> Sem r a
runGlobalStateInIO = interpretH $ \case
  Allocate a f -> do
    key <- sendM
         . atomicModifyIORef' unsafeGlobalIOAnyStore
         $ allocS a
    key' <- pureT key
    f' <- bindT f
    r <- raise $ runGlobalStateInIO $ f' key'
    sendM $ atomicModifyIORef'Void unsafeGlobalIOAnyStore $ freeS key
    pure r
  GetGlobal key -> do
    d <- sendM $ readIORef unsafeGlobalIOAnyStore
    pureT $ getS (fiddleKey key) d
  PutGlobal key a -> do
    sendM . atomicModifyIORef'Void unsafeGlobalIOAnyStore $ putS (fiddleKey key) a
    getInitialStateT


atomicModifyIORef'Void :: IORef a -> (a -> a) -> IO ()
atomicModifyIORef'Void ref f =
  atomicModifyIORef' ref $ \a -> (f a, ())

unsafeGlobalIOAnyStore :: IORef (AnyStore Any)
unsafeGlobalIOAnyStore =
  unsafePerformIO $ newIORef emptyS
{-# NOINLINE unsafeGlobalIOAnyStore #-}

