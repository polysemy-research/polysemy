{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Error
  ( -- * Effect
    Error (..)

    -- * Actions
  , throw
  , catch

    -- * Interpretations
  , runError
  , runErrorAsAnother
  , runErrorInIO
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Trans.Except as E
import           Data.Bifunctor (first)
import           Data.Typeable
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Union


-- data Error e m a where
--   Throw :: e -> Error e m a
--   Catch :: ∀ e m a. m a -> (e -> m a) -> Error e m a

data Error e m a = Throw e

makeSem ''Error


hush :: Either e a -> Maybe a
hush (Right a) = Just a
hush (Left _) = Nothing


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
            (either (pure . Left) runError_b)
            hush
            x
    Right (Yo (Throw e) _ _ _ _) -> E.throwE e
    Right (Yo (Catch try handle) s d y _) ->
      E.ExceptT $ usingSem k $ do
        ma <- runError_b $ d $ try <$ s
        case ma of
          Right a -> pure . Right $ y a
          Left e -> do
            ma' <- runError_b $ d $ (<$ s) $ handle e
            case ma' of
              Left e' -> pure $ Left e'
              Right a -> pure . Right $ y a
{-# INLINE runError #-}

runError_b
    :: Sem (Error e ': r) a
    -> Sem r (Either e a)
runError_b = runError
{-# NOINLINE runError_b #-}


------------------------------------------------------------------------------
-- | Transform one 'Error' into another. This function can be used to aggregate
-- multiple errors into a single type.
--
-- @since 0.2.2.0
runErrorAsAnother
  :: Member (Error e2) r
  => (e1 -> e2)
  -> Sem (Error e1 ': r) a
  -> Sem r a
runErrorAsAnother f = interpretH $ \case
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
{-# INLINE runErrorAsAnother #-}


newtype WrappedExc e = WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => X.Exception (WrappedExc e)


------------------------------------------------------------------------------
-- | Run an 'Error' effect as an 'IO' 'X.Exception'. This interpretation is
-- significantly faster than 'runError', at the cost of being less flexible.
runErrorInIO
    :: ( Typeable e
       , Member (Lift IO) r
       )
    => (∀ x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is
       -- likely some combination of 'runM' and other interpters composed via
       -- '.@'.
    -> Sem (Error e ': r) a
    -> Sem r (Either e a)
runErrorInIO lower
    = sendM
    . fmap (first unwrapExc)
    . X.try
    . (lower .@ runErrorAsExc)
{-# INLINE runErrorInIO #-}


runErrorAsExc
    :: forall e r a. ( Typeable e
       , Member (Lift IO) r
       )
    => (∀ x. Sem r x -> IO x)
    -> Sem (Error e ': r) a
    -> Sem r a
runErrorAsExc lower = interpretH $ \case
  Throw e -> sendM $ X.throwIO $ WrappedExc e
  Catch try handle -> do
    is <- getInitialStateT
    t  <- runT try
    h  <- bindT handle
    let runIt = lower . runErrorAsExc_b lower
    sendM $ X.catch (runIt t) $ \(se :: WrappedExc e) ->
      runIt $ h $ unwrapExc se <$ is
{-# INLINE runErrorAsExc #-}


runErrorAsExc_b
    :: ( Typeable e
       , Member (Lift IO) r
       )
    => (∀ x. Sem r x -> IO x)
    -> Sem (Error e ': r) a
    -> Sem r a
runErrorAsExc_b = runErrorAsExc
{-# NOINLINE runErrorAsExc_b #-}

