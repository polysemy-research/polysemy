{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Error
  ( Error (..)
  , throw
  , catch
  , runError
  ) where

import qualified Control.Monad.Trans.Except as E
import           Polysemy
import           Polysemy.Union
import           Polysemy.Effect.New


data Error e m a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

throw :: Member (Error e) r => e -> Semantic r a
throw = send . Throw

catch :: Member (Error e) r => Semantic r a -> (e -> Semantic r a) -> Semantic r a
catch ma h = send $ Catch ma h


runError :: Semantic (Error e ': r) a -> Semantic r (Either e a)
runError (Semantic m) = Semantic $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left x -> E.ExceptT $ k $
      weave (Right ()) (either (pure . Left) runError) x
    Right (Yo (Throw e) _ _ _) -> E.throwE e
    Right (Yo (Catch try handle) s d y) ->
      E.ExceptT $ usingSemantic k $ do
        ma <- runError $ d $ try <$ s
        case ma of
          Right a -> pure . Right $ y a
          Left e -> do
            ma' <- runError $ d $ (<$ s) $ handle e
            case ma' of
              Left e' -> pure $ Left e'
              Right a -> pure . Right $ y a

