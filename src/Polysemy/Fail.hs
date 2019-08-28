{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.Fail
  ( -- * Effect
    Fail(..)

    -- * Interpretations
  , runFail
  , failToError
  , failToNonDet
  , failToEmbed
  ) where

import Control.Applicative
import Polysemy
import Polysemy.Fail.Type
import Polysemy.Error
import Polysemy.NonDet
import Control.Monad.Fail as Fail

------------------------------------------------------------------------------
-- | Run a 'Fail' effect purely.
runFail :: Sem (Fail ': r) a
        -> Sem r (Either String a)
runFail = runError . reinterpret (\(Fail s) -> throw s)
{-# INLINE runFail #-}

------------------------------------------------------------------------------
-- | Transform a 'Fail' effect into an @'Error' e@ effect,
-- through providing a function for transforming any failure
-- to an exception.
failToError :: Member (Error e) r
            => (String -> e)
            -> Sem (Fail ': r) a
            -> Sem r a
failToError f = interpret $ \(Fail s) -> throw (f s)
{-# INLINE failToError #-}

------------------------------------------------------------------------------
-- | Transform a 'Fail' effect into a 'NonDet' effect,
-- through mapping any failure to 'empty'.
failToNonDet :: Member NonDet r
             => Sem (Fail ': r) a
             -> Sem r a
failToNonDet = interpret $ \(Fail _) -> empty
{-# INLINE failToNonDet #-}

------------------------------------------------------------------------------
-- | Run a 'Fail' effect in terms of an underlying 'MonadFail' instance.
failToEmbed :: forall m r a
             . (Member (Embed m) r, MonadFail m)
            => Sem (Fail ': r) a
            -> Sem r a
failToEmbed = interpret $ \(Fail s) -> embed @m (Fail.fail s)
{-# INLINE failToEmbed #-}
