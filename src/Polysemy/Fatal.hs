{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Polysemy.Fatal
  ( -- * Effect
    Fatal (..)

    -- * Actions
  , fatal
  , fatalFromEither
  , fatalFromEitherM
  , fatalFromException
  , fatalFromExceptionVia
  , fatalFromExceptionSem
  , fatalFromExceptionSemVia
  , noteFatal

    -- * Interpretations
  , runFatal
  , mapFatal
  , fatalToError
  , fatalToIOFinal
  ) where

import qualified Control.Exception as X
import           Control.Monad
import qualified Control.Monad.Trans.Except as E
import           Data.Typeable
import           Polysemy
import           Polysemy.Error
import           Polysemy.Final
import           Polysemy.Internal
import           Polysemy.Internal.Union


data Fatal e m a where
  Fatal :: e -> Fatal e m a

makeSem ''Fatal

------------------------------------------------------------------------------
-- | Upgrade an 'Either' into an 'Fatal' effect.
--
-- @since 0.5.1.0
fatalFromEither
    :: Member (Fatal e) r
    => Either e a
    -> Sem r a
fatalFromEither (Left e) = fatal e
fatalFromEither (Right a) = pure a
{-# INLINABLE fatalFromEither #-}

------------------------------------------------------------------------------
-- | A combinator doing 'embed' and 'fromEither' at the same time. Useful for
-- interoperating with 'IO'.
--
-- @since 0.5.1.0
fatalFromEitherM
    :: forall e m r a
     . ( Member (Fatal e) r
       , Member (Embed m) r
       )
    => m (Either e a)
    -> Sem r a
fatalFromEitherM = fatalFromEither <=< embed
{-# INLINABLE fatalFromEitherM #-}


------------------------------------------------------------------------------
-- | Lift an exception generated from an 'IO' action into an 'Fatal'.
fatalFromException
    :: forall e r a
     . ( X.Exception e
       , Member (Fatal e) r
       , Member (Embed IO) r
       )
    => IO a
    -> Sem r a
fatalFromException = fatalFromExceptionVia @e id
{-# INLINABLE fatalFromException #-}


------------------------------------------------------------------------------
-- | Like 'fromException', but with the ability to transform the exception
-- before turning it into an 'Fatal'.
fatalFromExceptionVia
    :: ( X.Exception exc
       , Member (Fatal err) r
       , Member (Embed IO) r
       )
    => (exc -> err)
    -> IO a
    -> Sem r a
fatalFromExceptionVia f m = do
  r <- embed $ X.try m
  case r of
    Left e -> fatal $ f e
    Right a -> pure a
{-# INLINABLE fatalFromExceptionVia #-}

------------------------------------------------------------------------------
-- | Run a @Sem r@ action, converting any 'IO' exception generated by it into an 'Fatal'.
fatalFromExceptionSem
    :: forall e r a
     . ( X.Exception e
       , Member (Fatal e) r
       , Member (Final IO) r
       )
    => Sem r a
    -> Sem r a
fatalFromExceptionSem = fatalFromExceptionSemVia @e id
{-# INLINABLE fatalFromExceptionSem #-}


------------------------------------------------------------------------------
-- | Like 'fromExceptionSem', but with the ability to transform the exception
-- before turning it into an 'Fatal'.
fatalFromExceptionSemVia
    :: ( X.Exception exc
       , Member (Fatal err) r
       , Member (Final IO) r
       )
    => (exc -> err)
    -> Sem r a
    -> Sem r a
fatalFromExceptionSemVia f m = do
  r <- controlFinal $ \lower ->
    lower (fmap Right m) `X.catch` (lower . return . Left)
  case r of
    Left e -> fatal $ f e
    Right a -> pure a
{-# INLINABLE fatalFromExceptionSemVia #-}


------------------------------------------------------------------------------
-- | Attempt to extract a @'Just' a@ from a @'Maybe' a@, throwing the
-- provided exception upon 'Nothing'.
noteFatal :: Member (Fatal e) r => e -> Maybe a -> Sem r a
noteFatal e Nothing  = fatal e
noteFatal _ (Just a) = pure a
{-# INLINABLE noteFatal #-}


------------------------------------------------------------------------------
-- | Run an 'Fatal' effect in the style of
-- 'Control.Monad.Trans.Except.ExceptT'.
runFatal
    :: Sem (Fatal e ': r) a
    -> Sem r (Either e a)
runFatal (Sem m) = Sem $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left x ->
      liftHandlerWithNat (E.ExceptT . runFatal) k x
    Right (Weaving (Fatal e) _ _ _) -> E.throwE e
{-# INLINE runFatal #-}


------------------------------------------------------------------------------
-- | Transform one 'Fatal' into another. This function can be used to aggregate
-- multiple fatals into a single type.
--
-- @since 1.0.0.0
mapFatal
  :: forall e1 e2 r a
   . Member (Fatal e2) r
  => (e1 -> e2)
  -> Sem (Fatal e1 ': r) a
  -> Sem r a
mapFatal f = transform (\(Fatal e) -> Fatal (f e))
{-# INLINE mapFatal #-}


newtype WrappedExc e = WrappedExc { _unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => X.Exception (WrappedExc e)


------------------------------------------------------------------------------
-- | Run an 'Fatal' effect as an 'IO' 'X.Exception' through final 'IO'. This
-- interpretation is significantly faster than 'runFatal'.
--
-- /Note/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Fatal' effects
-- interpreted this way. See 'Final'.
--
-- @since 1.2.0.0
fatalToIOFinal
    :: ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Fatal e ': r) a
    -> Sem r (Either e a)
fatalToIOFinal sem = controlFinal $ \lower -> do
    lower (Right <$> runFatalAsExcFinal sem)
  `X.catch` \(WrappedExc e) ->
    lower $ return $ Left e
{-# INLINE fatalToIOFinal #-}


runFatalAsExcFinal
    :: forall e r a
    .  ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Fatal e ': r) a
    -> Sem r a
runFatalAsExcFinal = interpretFinal @IO $ \case
  Fatal e   -> embed $ X.throwIO $ WrappedExc e
{-# INLINE runFatalAsExcFinal #-}


fatalToError
    :: Member (Error e) r
    => Sem (Fatal e ': r) a
    -> Sem r a
fatalToError = interpret $ \case
  Fatal e -> throw e
{-# INLINE fatalToError #-}
