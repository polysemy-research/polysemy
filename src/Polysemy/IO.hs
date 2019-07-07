{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.IO
  ( -- * Interpretations
    runIO
  , runEmbedded
  ) where

import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed
import Polysemy.Internal
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | __If you trying to run 'Sem' in 'IO', the function you want is 'runM'.__
--
-- The 'MonadIO' class is conceptually an interpretation of 'IO' to some
-- other monad. This function reifies that intuition, by transforming an 'IO'
-- effect into some other 'MonadIO'.
--
-- This function is especially useful when using the 'MonadIO' instance for
-- 'Sem' instance.
--
-- Make sure to type-apply the desired 'MonadIO' instance when using 'runIO'.
--
-- @since 0.1.1.0
--
-- ==== Example
--
-- @
-- foo :: PandocIO ()
-- foo = 'runM' . 'runIO' @PandocIO $ do
--   'liftIO' $ putStrLn "hello from polysemy"
-- @
--
runIO
    :: forall m r a
     . ( MonadIO m
       , Member (Embed m) r
       )
    => Sem (Embed IO ': r) a
    -> Sem r a
runIO = runEmbed $ liftIO @m
{-# INLINE runIO #-}


------------------------------------------------------------------------------
-- | Given some @'MonadIO' m@, interpret all @'Embed' m@ actions in that monad
-- at once. This is useful for interpreting effects like databases, which use
-- their own monad for describing actions.
--
-- This function creates a thread, and so should be compiled with @-threaded@.
--
-- @since 0.6.0.0
runEmbedded
    :: ( MonadIO m
       , LastMember (Embed IO) r
       )
    => (forall x. m x -> IO x)  -- ^ The means of running this monad.
    -> Sem (Embed m ': r) a
    -> Sem r a
runEmbedded run_m (Sem m) = withLowerToIO $ \lower _ ->
  run_m $ m $ \u ->
    case decomp u of
      Left x -> liftIO
              . lower
              . liftSem
              $ hoist (runEmbedded run_m) x

      Right (Weaving (Embed wd) s _ y _) ->
        fmap y $ fmap (<$ s) wd

