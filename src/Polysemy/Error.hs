{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

{-# OPTIONS_GHC -Wall #-}

module Polysemy.Error
  ( Error (..)
  , throw
  , catch
  , runError
  ) where

import qualified Control.Monad.Trans.Except as E
import           Polysemy
import           Polysemy.Interpretation
import           Polysemy.Internal.Effect
import           Polysemy.Internal.Union


data Error e m a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a

makeSemantic ''Error


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

