{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Error
  ( Error (..)
  , throw
  , catch
  , runError
  , runErrorInIO
  ) where

import qualified Control.Exception as X
import qualified Control.Monad.Trans.Except as E
import           Data.Bifunctor (first)
import           Data.Typeable
import           Polysemy
import           Polysemy.Internal
import           Polysemy.Internal.Effect
import           Polysemy.Internal.Union


data Error e m a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

makeSemantic ''Error


runError
    :: Typeable e
    => Semantic (Error e ': r) a
    -> Semantic r (Either e a)
runError (Semantic m) = Semantic $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left x -> E.ExceptT $ k $
      weave (Right ()) (either (pure . Left) runError_b) x
    Right (Yo (Throw e) _ _ _) -> E.throwE e
    Right (Yo (Catch try handle) s d y) ->
      E.ExceptT $ usingSemantic k $ do
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
    :: Typeable e
    => Semantic (Error e ': r) a
    -> Semantic r (Either e a)
runError_b = runError
{-# NOINLINE runError_b #-}


newtype WrappedExc e = WrappedExc { unwrapExc :: e }
  deriving (Typeable)

instance Typeable e => Show (WrappedExc e) where
  show = mappend "WrappedExc: " . show . typeRep

instance (Typeable e) => X.Exception (WrappedExc e)


runErrorInIO
    :: ( Typeable e
       , Member (Lift IO) r
       )
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Error e ': r) a
    -> Semantic r (Either e a)
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
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Error e ': r) a
    -> Semantic r a
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
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Error e ': r) a
    -> Semantic r a
runErrorAsExc_b = runErrorAsExc
{-# NOINLINE runErrorAsExc_b #-}

