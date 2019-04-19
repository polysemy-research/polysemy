{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Resource
  ( -- * Effect
    Resource (..)

    -- * Actions
  , bracket

    -- * Interpretations
  , runResource
  ) where

import qualified Control.Exception as X
import           Polysemy


------------------------------------------------------------------------------
-- | An effect capable of providing 'X.bracket' semantic. Interpreters for this
-- will successfully run the deallocation action even in the presence of other
-- short-circuiting effects.
data Resource m a where
  Bracket
    :: m a
       -- ^ Action to allocate a resource.
    -> (a -> m ())
       -- ^ Action to cleanup the resource. This is guaranteed to be
       -- called.
    -> (a -> m b)
       -- ^ Action which uses the resource.
    -> Resource m b

makeSemantic ''Resource


------------------------------------------------------------------------------
-- | Run a 'Resource' effect via in terms of 'X.bracket'.
runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
       -- ^ Strategy for lowering a 'Semantic' action down to 'IO'. This is
       -- likely some combination of 'runM' and other interpreters composed via
       -- '.@'.
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let runIt :: Semantic (Resource ': r) x -> IO x
        runIt = finish .@ runResource

    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)

