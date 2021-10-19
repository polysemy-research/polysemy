{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.IO
  ( -- * Interpretations
    embedToMonadIO
  ) where

import Control.Monad.IO.Class
import Polysemy
import Polysemy.Embed


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

