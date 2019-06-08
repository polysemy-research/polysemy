{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Polysemy.IO
  ( -- * Interpretations
    runIO
  ) where

import Polysemy
import Control.Monad.IO.Class


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
    :: ( MonadIO m
       , Member (Lift m) r
       )
    => Sem (Lift IO ': r) a
    -> Sem r a
runIO = interpret $ sendM . liftIO . unLift
{-# INLINE runIO #-}

