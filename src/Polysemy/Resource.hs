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
-- | An effect capable of providing 'X.bracket' semantics. Interpreters for this
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

makeSem ''Resource


------------------------------------------------------------------------------
-- | Run a 'Resource' effect via in terms of 'X.bracket'.
runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is likely
       -- some combination of 'runM' and other interpreters composed via '.@'.
    -> Sem (Resource ': r) a
    -> Sem r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let runIt :: Sem (Resource ': r) x -> IO x
        runIt = finish .@ runResource

    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)

