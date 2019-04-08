{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UnicodeSyntax   #-}

module Polysemy.Resource
  ( Resource (..)
  , bracket
  , runResource
  ) where

import qualified Control.Exception as X
import           Polysemy
import           Polysemy.Effect.New


data Resource m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b

makeSemantic ''Resource


runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- start    alloc
    d <- continue dealloc
    u <- continue use

    let runIt :: Semantic (Resource ': r) x -> IO x
        runIt = finish . runResource finish

    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)

