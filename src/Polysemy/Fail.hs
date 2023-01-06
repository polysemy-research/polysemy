{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Description: 'Fail' interpreters
module Polysemy.Fail
  ( -- * Effect
    Fail(..)

    -- * Interpretations
  , runFail
  , failToFatal
  , failToError
  , failToNonDet
  , failToEmbed
  ) where

import Data.Coerce
import Polysemy
import Polysemy.Fatal
import Polysemy.Fail.Type
import Polysemy.Error
import Polysemy.NonDet
import Control.Monad.Fail as Fail

------------------------------------------------------------------------------
-- | Run a 'Fail' effect purely.
runFail :: Sem (Fail ': r) a
        -> Sem r (Either String a)
runFail = runError . rewrite (coerce (Throw :: String -> Error String z x)
                              :: forall z x. Fail z x -> Error String z x)

------------------------------------------------------------------------------
-- | Transform a 'Fail' effect into an @'Error' e@ effect,
-- through providing a function for transforming any failure
-- to an exception.
failToError :: Member (Error e) r
            => (String -> e)
            -> Sem (Fail ': r) a
            -> Sem r a
failToError f = transform (\(Fail e) -> Throw (f e))

------------------------------------------------------------------------------
-- | Transform a 'Fail' effect into a @'Fatal' e@ effect,
-- through providing a function for transforming any failure
-- to an exception.
failToFatal :: forall e r a
             . Member (Fatal e) r
            => (String -> e)
            -> Sem (Fail ': r) a
            -> Sem r a
failToFatal f = transform @_ @(Fatal e) (coerce f)
{-# INLINE failToError #-}


------------------------------------------------------------------------------
-- | Transform a 'Fail' effect into a 'NonDet' effect,
-- through mapping any failure to 'empty'.
failToNonDet :: Member NonDet r
             => Sem (Fail ': r) a
             -> Sem r a
failToNonDet = transform $ \(Fail _) -> Empty

------------------------------------------------------------------------------
-- | Run a 'Fail' effect in terms of an underlying 'MonadFail' instance.
failToEmbed :: forall m r a
             . (Member (Embed m) r, MonadFail m)
            => Sem (Fail ': r) a
            -> Sem r a
failToEmbed = transform $ \(Fail s) -> Embed @m (Fail.fail s)
