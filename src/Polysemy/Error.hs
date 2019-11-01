{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TemplateHaskell     #-}

module Polysemy.Error
  ( -- * Effect
    Error (..)

    -- * Actions
  , throw
  , catch
  , fromEither
  , fromEitherM
  , fromException
  , fromExceptionVia

    -- * Interpretations
  , runError
  , mapError
  , errorToIOFinal
  , lowerError
  ) where

import qualified Control.Exception as X
import           Control.Monad
import qualified Control.Monad.Trans.Except as E
import           Data.Bifunctor (first)
import           Data.Typeable
import           Polysemy
import           Polysemy.Final
import           Polysemy.Internal
import           Polysemy.Internal.Union


data Error e m a where
  Throw :: e -> Error e m a
  Catch :: ∀ e m a. m a -> (e -> m a) -> Error e m a

makeSem ''Error


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing


------------------------------------------------------------------------------
-- | Upgrade an 'Either' into an 'Error' effect.
--
-- @since 0.5.1.0
fromEither
    :: Member (Error e) r
    => Either e a
    -> Sem r a
fromEither (Left e) = throw e
fromEither (Right a) = pure a


------------------------------------------------------------------------------
-- | A combinator doing 'embed' and 'fromEither' at the same time. Useful for
-- interoperating with 'IO'.
--
-- @since 0.5.1.0
fromEitherM
    :: forall e m r a
     . ( Member (Error e) r
       , Member (Embed m) r
       )
    => m (Either e a)
    -> Sem r a
fromEitherM = fromEither <=< embed


------------------------------------------------------------------------------
-- | Lift an exception generated from an 'IO' action into an 'Error'.
fromException
    :: forall e r a
     . ( X.Exception e
       , Member (Error e) r
       , Member (Embed IO) r
       )
    => IO a
    -> Sem r a
fromException = fromExceptionVia @e id


------------------------------------------------------------------------------
-- | Like 'fromException', but with the ability to transform the exception
-- before turning it into an 'Error'.
fromExceptionVia
    :: ( X.Exception exc
       , Member (Error err) r
       , Member (Embed IO) r
       )
    => (exc -> err)
    -> IO a
    -> Sem r a
fromExceptionVia f m = do
  r <- embed $ X.try m
  case r of
    Left e -> throw $ f e
    Right a -> pure a


------------------------------------------------------------------------------
-- | Run an 'Error' effect in the style of
-- 'Control.Monad.Trans.Except.ExceptT'.
runError
    :: Sem (Error e ': r) a
    -> Sem r (Either e a)
runError (Sem m) = Sem $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left x -> E.ExceptT $ k $
      weave (Right ())
            (either (pure . Left) runError)
            hush
            x
    Right (Weaving (Throw e) _ _ _ _) -> E.throwE e
    Right (Weaving (Catch try handle) s d y _) ->
      E.ExceptT $ usingSem k $ do
        ma <- runError $ d $ try <$ s
        case ma of
          Right a -> pure . Right $ y a
          Left e -> do
            ma' <- runError $ d $ (<$ s) $ handle e
            case ma' of
              Left e' -> pure $ Left e'
              Right a -> pure . Right $ y a
{-# INLINE runError #-}


------------------------------------------------------------------------------
-- | Transform one 'Error' into another. This function can be used to aggregate
-- multiple errors into a single type.
--
-- @since 1.0.0.0
mapError
  :: forall e1 e2 r a
   . Member (Error e2) r
  => (e1 -> e2)
  -> Sem (Error e1 ': r) a
  -> Sem r a
mapError f = interpretH $ \case
  Throw e -> throw $ f e
  Catch action handler -> do
    a  <- runT action
    h  <- bindT handler

    mx <- raise $ runError a
    case mx of
      Right x -> pure x
      Left e -> do
        istate <- getInitialStateT
        mx' <- raise $ runError $ h $ e <$ istate
        case mx' of
          Right x -> pure x
          Left e' -> throw $ f e'
{-# INLINE mapError #-}


newtype WrappedExc e = WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => X.Exception (WrappedExc e)


------------------------------------------------------------------------------
-- | Run an 'Error' effect as an 'IO' 'X.Exception' through final 'IO'. This
-- interpretation is significantly faster than 'runError'.
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Error' effects
-- interpreted this way. See 'Final'.
--
-- @since 1.2.0.0
errorToIOFinal
    :: ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r (Either e a)
errorToIOFinal sem = withStrategicToFinal @IO $ do
  m' <- runS (runErrorAsExcFinal sem)
  s  <- getInitialStateS
  pure $
    either
      ((<$ s) . Left . unwrapExc)
      (fmap Right)
    <$> X.try m'
{-# INLINE errorToIOFinal #-}

runErrorAsExcFinal
    :: forall e r a
    .  ( Typeable e
       , Member (Final IO) r
       )
    => Sem (Error e ': r) a
    -> Sem r a
runErrorAsExcFinal = interpretFinal $ \case
  Throw e   -> pure $ X.throwIO $ WrappedExc e
  Catch m h -> do
    m' <- runS m
    h' <- bindS h
    s  <- getInitialStateS
    pure $ X.catch m' $ \(se :: WrappedExc e) ->
      h' (unwrapExc se <$ s)
{-# INLINE runErrorAsExcFinal #-}

------------------------------------------------------------------------------
-- | Run an 'Error' effect as an 'IO' 'X.Exception'. This interpretation is
-- significantly faster than 'runError', at the cost of being less flexible.
--
-- @since 1.0.0.0
lowerError
    :: ( Typeable e
       , Member (Embed IO) r
       )
    => (∀ x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is
       -- likely some combination of 'runM' and other interpreters composed via
       -- '.@'.
    -> Sem (Error e ': r) a
    -> Sem r (Either e a)
lowerError lower
    = embed
    . fmap (first unwrapExc)
    . X.try
    . (lower .@ runErrorAsExc)
{-# INLINE lowerError #-}
{-# DEPRECATED lowerError "Use 'errorToIOFinal' instead" #-}


-- TODO(sandy): Can we use the new withLowerToIO machinery for this?
runErrorAsExc
    :: forall e r a. ( Typeable e
       , Member (Embed IO) r
       )
    => (∀ x. Sem r x -> IO x)
    -> Sem (Error e ': r) a
    -> Sem r a
runErrorAsExc lower = interpretH $ \case
  Throw e -> embed $ X.throwIO $ WrappedExc e
  Catch try handle -> do
    is <- getInitialStateT
    t  <- runT try
    h  <- bindT handle
    let runIt = lower . runErrorAsExc lower
    embed $ X.catch (runIt t) $ \(se :: WrappedExc e) ->
      runIt $ h $ unwrapExc se <$ is
{-# INLINE runErrorAsExc #-}
