{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.IO
  ( -- * Interpretations
    embedToMonadIO
  , lowerEmbedded
  ) where

import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed
import Polysemy.Internal
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- The 'MonadIO' class is conceptually an interpretation of 'IO' to some
-- other monad. This function reifies that intuition, by transforming an 'IO'
-- effect into some other 'MonadIO'.
--
-- This function is especially useful when using the 'MonadIO' instance for
-- 'Sem' instance.
--
-- Make sure to type-apply the desired 'MonadIO' instance when using
-- 'embedToMonadIO'.
--
-- @since 1.0.0.0
--
-- ==== Example
--
-- @
-- foo :: PandocIO ()
-- foo = 'runM' . 'embedToMonadIO' @PandocIO $ do
--   'liftIO' $ putStrLn "hello from polysemy"
-- @
--
embedToMonadIO
    :: forall m r a
     . ( MonadIO m
       , Member (Embed m) r
       )
    => Sem (Embed IO ': r) a
    -> Sem r a
embedToMonadIO = runEmbedded $ liftIO @m
{-# INLINE embedToMonadIO #-}


------------------------------------------------------------------------------
-- | Given some @'MonadIO' m@, interpret all @'Embed' m@ actions in that monad
-- at once. This is useful for interpreting effects like databases, which use
-- their own monad for describing actions.
--
-- This function creates a thread, and so should be compiled with @-threaded@.
--
-- @since 1.0.0.0
lowerEmbedded
    :: ( MonadIO m
       , Member (Embed IO) r
       )
    => (forall x. m x -> IO x)  -- ^ The means of running this monad.
    -> Sem (Embed m ': r) a
    -> Sem r a
lowerEmbedded run_m (Sem m) = withLowerToIO $ \lower _ ->
  run_m $ m $ \u ->
    case decomp u of
      Left x -> liftIO
              . lower
              . liftSem
              $ hoist (lowerEmbedded run_m) x

      Right (Weaving (Embed wd) s _ y _) ->
        fmap y $ fmap (<$ s) wd

