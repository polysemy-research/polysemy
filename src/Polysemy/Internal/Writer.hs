{-# LANGUAGE BangPatterns, TemplateHaskell, TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}
module Polysemy.Internal.Writer where

import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import qualified Control.Monad.Trans.Writer.Lazy as Lazy

import Data.Bifunctor (first)
import Data.Semigroup

import Polysemy
import Polysemy.Final

import Polysemy.Internal
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | An effect capable of emitting and intercepting messages.
data Writer o m a where
  Tell   :: o -> Writer o m ()
  Listen :: ∀ o m a. m a -> Writer o m (o, a)
  Pass   :: m (o -> o, a) -> Writer o m a

makeSem ''Writer

-- TODO(KingoftheHomeless): Research if this is more or less efficient than
-- using 'reinterpretH' + 'subsume'

-----------------------------------------------------------------------------
-- | Transform a @'Writer' o@ effect into a  @'Writer' ('Endo' o)@ effect,
-- right-associating all uses of '<>' for @o@.
--
-- This can be used together with 'raiseUnder' in order to create
-- @-AssocR@ variants out of regular 'Writer' interpreters.
--
-- @since 1.2.0.0
writerToEndoWriter
    :: (Monoid o, Member (Writer (Endo o)) r)
    => Sem (Writer o ': r) a
    -> Sem r a
writerToEndoWriter = interpretH $ \case
      Tell o   -> tell (Endo (o <>)) >>= pureT
      Listen m -> do
        m' <- writerToEndoWriter <$> runT m
        raise $ do
          (o, fa) <- listen m'
          return $ (,) (appEndo o mempty) <$> fa
      Pass m -> do
        ins <- getInspectorT
        m'  <- writerToEndoWriter <$> runT m
        raise $ pass $ do
          t <- m'
          let
            f' =
              maybe
                id
                (\(f, _) (Endo oo) -> let !o' = f (oo mempty) in Endo (o' <>))
                (inspect ins t)
          return (f', fmap snd t)
{-# INLINE writerToEndoWriter #-}


-- TODO(KingoftheHomeless): Make this mess more palatable
--
-- 'interpretFinal' is too weak for our purposes, so we
-- use 'interpretH' + 'withWeavingToFinal'.

------------------------------------------------------------------------------
-- | A variant of 'Polysemy.Writer.runWriterTVar' where an 'STM' action is
-- used instead of a 'TVar' to commit 'tell's.
runWriterSTMAction :: forall o r a
                          . (Member (Final IO) r, Monoid o)
                         => (o -> STM ())
                         -> Sem (Writer o ': r) a
                         -> Sem r a
runWriterSTMAction write = interpretH $ \case
  Tell o -> do
    t <- embedFinal $ atomically (write o)
    pureT t
  Listen m -> do
    m' <- runT m
    -- Using 'withWeavingToFinal' instead of 'withStrategicToFinal'
    -- here allows us to avoid using two additional 'embedFinal's in
    -- order to create the TVars.
    raise $ withWeavingToFinal $ \s wv _ -> mask $ \restore -> do
      -- See below to understand how this works
      tvar   <- newTVarIO mempty
      switch <- newTVarIO False
      fa     <-
        restore (wv (runWriterSTMAction (write' tvar switch) m' <$ s))
          `onException` commit tvar switch id
      o      <- commit tvar switch id
      return $ (fmap . fmap) (o, ) fa
  Pass m -> do
    m'  <- runT m
    ins <- getInspectorT
    raise $ withWeavingToFinal $ \s wv ins' -> mask $ \restore -> do
      tvar   <- newTVarIO mempty
      switch <- newTVarIO False
      t      <-
        restore (wv (runWriterSTMAction (write' tvar switch) m' <$ s))
          `onException` commit tvar switch id
      _      <- commit tvar switch
        (maybe id fst $ ins' t >>= inspect ins)
      return $ (fmap . fmap) snd t

  where
    {- KingoftheHomeless:
      'write'' is used by the argument computation to a 'listen' or 'pass'
      in order to 'tell', rather than directly using the 'write'.
      This is because we need to temporarily store its
      'tell's seperately in order for the 'listen'/'pass' to work
      properly. Once the 'listen'/'pass' completes, we 'commit' the
      changes done to the local tvar globally through 'write'.

      'commit' is protected by 'mask'+'onException'. Combine this
      with the fact that the 'withWeavingToFinal' can't be interrupted
      by pure errors emitted by effects (since these will be
      represented as part of the functorial state), and we
      guarantee that no writes will be lost if the argument computation
      fails for whatever reason.

      The argument computation to a 'listen'/'pass' may also spawn
      asynchronous computations which do 'tell's of their own.
      In order to make sure these 'tell's won't be lost once a
      'listen'/'pass' completes, a switch is used to
      control which tvar 'write'' writes to. The switch is flipped
      atomically together with commiting the writes of the local tvar
      as part of 'commit'. Once the switch is flipped,
      any asynchrounous computations spawned by the argument
      computation will write to the global tvar instead of the local
      tvar (which is no longer relevant), and thus no writes will be
      lost.
    -}
    write' :: TVar o
           -> TVar Bool
           -> o
           -> STM ()
    write' tvar switch = \o -> do
      useGlobal <- readTVar switch
      if useGlobal then
        write o
      else do
        s <- readTVar tvar
        writeTVar tvar $! s <> o

    commit :: TVar o
           -> TVar Bool
           -> (o -> o)
           -> IO o
    commit tvar switch f = atomically $ do
      o <- readTVar tvar
      let !o' = f o
      -- Likely redundant, but doesn't hurt.
      alreadyCommited <- readTVar switch
      unless alreadyCommited $
        write o'
      writeTVar switch True
      return o'
{-# INLINE runWriterSTMAction #-}


-- TODO (KingoftheHomeless):
-- Benchmark to see if switching to a more flexible variant
-- would incur a performance loss
interpretViaLazyWriter
  :: forall o e r a
   . Monoid o
  => (forall m x. Monad m => Weaving e (Lazy.WriterT o m) x -> Lazy.WriterT o m x)
  -> Sem (e ': r) a
  -> Sem r (o, a)
interpretViaLazyWriter f sem = Sem $ \(k :: forall x. Union r (Sem r) x -> m x) ->
  let
    go :: forall x. Sem (e ': r) x -> Lazy.WriterT o m x
    go = usingSem $ \u -> case decomp u of
      Right (Weaving e s wv ex ins) -> f $ Weaving e s (go . wv) ex ins
      Left g -> Lazy.WriterT $ do
        ~(o, a) <- k $
          weave
            (mempty, ())
            (\ ~(o, m) -> (fmap . first) (o <>) (interpretViaLazyWriter f m))
            (Just . snd)
            g
        return (a, o)
  in do
    ~(a,s) <- Lazy.runWriterT (go sem)
    return (s, a)
{-# INLINE interpretViaLazyWriter #-}
