{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Internal.Forklift where

import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Monad
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | A promise for interpreting an effect of the union @r@ in another thread.
--
-- TODO(sandy): @since
data Forklift r = forall a. Forklift
  { responseMVar :: MVar (Sem '[Lift IO] a)
  , request      :: Union r (Sem r) a
  }


------------------------------------------------------------------------------
-- | A strategy for automatically interpreting an entire stack of effects by
-- just shipping them off to some other interpretation context.
--
-- TODO(sandy): @since
runViaForklift
    :: LastMember (Lift IO) r
    => InChan (Forklift r)
    -> Sem r a
    -> Sem '[Lift IO] a
runViaForklift chan (Sem m) = Sem $ \k -> m $ \u -> do
  case decompLast u of
    Left x -> usingSem k $ join $ sendM $ do
      mvar <- newEmptyMVar
      writeChan chan $ Forklift mvar x
      takeMVar mvar
    Right y -> k $ hoist (runViaForklift_b chan) y
{-# INLINE runViaForklift #-}


runViaForklift_b
    :: LastMember (Lift IO) r
    => InChan (Forklift r)
    -> Sem r a
    -> Sem '[Lift IO] a
runViaForklift_b = runViaForklift
{-# NOINLINE runViaForklift_b #-}


------------------------------------------------------------------------------
-- | Run an effect stack all the way down to 'IO' by running it in a new
-- thread, and temporarily turning the current thread into an event poll.
--
-- This function creates a thread, and so should be compiled with @-threaded@.
--
-- TODO(sandy): @since
withLowerToIO
    :: LastMember (Lift IO) r
    => ((forall x. Sem r x -> IO x) -> IO () -> IO a)
       -- ^ A lambda that takes the lowering function, and a finalizing 'IO'
       -- action to mark a the forked thread as being complete. The finalizing
       -- action need not be called.
    -> Sem r a
withLowerToIO action = do
  (inchan, outchan) <- sendM newChan
  signal <- sendM newEmptyMVar

  res <- sendM $ A.async $ do
    a <- action (runM . runViaForklift inchan)
                (putMVar signal ())
    putMVar signal ()
    pure a

  let me = do
        raced <- sendM $ A.race (takeMVar signal) $ readChan outchan
        case raced of
          Left () -> sendM $ A.wait res
          Right (Forklift mvar req) -> do
            resp <- liftSem req
            sendM $ putMVar mvar $ pure resp
            me_b
      {-# INLINE me #-}

      me_b = me
      {-# NOINLINE me_b #-}

  me

