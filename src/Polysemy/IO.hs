{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.IO
  ( -- * Interpretations
    runIO
  ) where

import Polysemy
import Control.Monad.IO.Class


------------------------------------------------------------------------------
-- | The 'MonadIO' class is conceptually an interpretation of 'IO' to some
-- other monad. This function reifies that intuition, by transforming an 'IO'
-- effect into some other 'MonadIO'.
--
-- This function is especially useful when using the 'MonadIO' instance for
-- 'Semantic' instance.
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
       , Member (Lift m) r
       )
    => Semantic (Lift IO ': r) a
    -> Semantic r a
runIO = interpret $ sendM . liftIO @m . unLift
{-# INLINE runIO #-}

