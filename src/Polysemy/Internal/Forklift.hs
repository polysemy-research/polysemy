{-# LANGUAGE NumDecimals     #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Forklift where

import qualified Control.Concurrent.Async as A
import           Control.Concurrent.Chan.Unagi
import           Control.Concurrent.MVar
import           Control.Exception
import           Polysemy.Internal
import           Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | A promise for interpreting an effect of the union @r@ in another thread.
--
-- @since 0.5.0.0
data Forklift r = forall a. Forklift
  { responseMVar :: MVar a
  , request      :: Union r (Sem r) a
  }


------------------------------------------------------------------------------
-- | A strategy for automatically interpreting an entire stack of effects by
-- just shipping them off to some other interpretation context.
--
-- @since 0.5.0.0
runViaForklift
    :: Member (Embed IO) r
    => InChan (Forklift r)
    -> Sem r a
    -> IO a
runViaForklift chan = usingSem $ \u -> do
  case prj u of
    Just (Weaving (Embed m) s _ ex _) ->
      ex . (<$ s) <$> m
    _ -> do
      mvar <- newEmptyMVar
      writeChan chan $ Forklift mvar u
      takeMVar mvar
{-# INLINE runViaForklift #-}



------------------------------------------------------------------------------
-- | Run an effect stack all the way down to 'IO' by running it in a new
-- thread, and temporarily turning the current thread into an event poll.
--
-- This function creates a thread, and so should be compiled with @-threaded@.
--
-- @since 0.5.0.0
withLowerToIO
    :: Member (Embed IO) r
    => ((forall x. Sem r x -> IO x) -> IO () -> IO a)
       -- ^ A lambda that takes the lowering function, and a finalizing 'IO'
       -- action to mark a the forked thread as being complete. The finalizing
       -- action need not be called.
    -> Sem r a
withLowerToIO action = do
  (inchan, outchan) <- embed newChan
  signal <- embed newEmptyMVar

  res <- embed $ A.async $ do
    a <- action (runViaForklift inchan)
                (putMVar signal ())
          `finally` (putMVar signal ())
    pure a

  let me = do
        raced <- embed $ A.race (takeMVar signal) $ readChan outchan
        case raced of
          Left () -> embed $ A.wait res
          Right (Forklift mvar req) -> do
            resp <- liftSem req
            embed $ putMVar mvar $ resp
            me_b
      {-# INLINE me #-}

      me_b = me
      {-# NOINLINE me_b #-}

  me

