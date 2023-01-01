{-# LANGUAGE TemplateHaskell #-}

-- | Description: The 'Reader' effect and its interpreters
module Polysemy.Reader
  ( -- * Effect
    Reader (..)

    -- * Actions
  , ask
  , asks
  , local

    -- * Interpretations
  , runReader

    -- * Interpretations for Other Effects
  , inputToReader
  ) where

import Polysemy
import Polysemy.Input


------------------------------------------------------------------------------
-- | The Polysemy port of 'Control.Monad.Trans.Reader.ReaderT'.
--
-- __Note that this is probably not the effect you are looking for.__ You
-- probably want 'Polysemy.Input.Input' instead, which is like 'Reader' but
-- without 'local'.
--
-- If you are trying to emulate anything akin to the @ReaderT IO@ pattern, note
-- that it is /not recommended/ in Polysemy. Instead, your experience will be
-- much more joyful if you avoid @IO@ entirely and think deeply about the
-- lawful chunks of your program that can be turned into effects.
data Reader i m a where
  -- | Get the environment.
  Ask   :: Reader i m i
  -- | Transform the environment.
  Local :: (i -> i) -> m a -> Reader i m a

makeSem ''Reader


------------------------------------------------------------------------------
-- | Apply a function to the environment and return the result.
asks :: forall i j r. Member (Reader i) r => (i -> j) -> Sem r j
asks f = f <$> ask
{-# INLINABLE asks #-}


------------------------------------------------------------------------------
-- | Run a 'Reader' effect with a constant value.
runReader :: i -> Sem (Reader i ': r) a -> Sem r a
runReader i = interpretH $ \case
  Ask -> return i
  Local f m -> runReader (f i) (runH' m)


------------------------------------------------------------------------------
-- | Transform an 'Input' effect into a 'Reader' effect.
--
-- @since 1.0.0.0
inputToReader :: forall i r a. Member (Reader i) r => Sem (Input i ': r) a -> Sem r a
inputToReader = transform @_ @(Reader i) (\Input -> Ask)
