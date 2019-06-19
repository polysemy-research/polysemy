{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Resource
  ( -- * Effect
    Resource (..)

    -- * Actions
  , bracket
  , bracketOnError
  , finally
  , onException

    -- * Interpretations
  , runResource
  , runResourceInIO
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
       -- Action to allocate a resource.
    -> (a -> m c)
       -- Action to cleanup the resource. This is guaranteed to be
       -- called.
    -> (a -> m b)
       -- Action which uses the resource.
    -> Resource m b
  BracketOnError
    :: m a
       -- Action to allocate a resource.
    -> (a -> m c)
       -- Action to cleanup the resource. This will only be called if the
       -- "use" block fails.
    -> (a -> m b)
       -- Action which uses the resource.
    -> Resource m b

makeSem ''Resource


------------------------------------------------------------------------------
-- | Like 'bracket', but for the simple case of one computation to run
-- afterward.
--
-- @since 0.4.0.0
finally
    :: Member Resource r
    => Sem r a -- ^ computation to run first
    -> Sem r b -- ^ computation to run afterward (even if an exception was raised)
    -> Sem r a
finally act end = bracket (pure ()) (pure end) (const act)


------------------------------------------------------------------------------
-- | Like 'bracketOnError', but for the simple case of one computation to run
-- afterward.
--
-- @since 0.4.0.0
onException
    :: Member Resource r
    => Sem r a -- ^ computation to run first
    -> Sem r b -- ^ computation to run afterward if an exception was raised
    -> Sem r a
onException act end = bracketOnError (pure ()) (const end) (const act)


------------------------------------------------------------------------------
-- | Run a 'Resource' effect via in terms of 'X.bracket'.
--
-- __Note:__ This function used to be called @runResource@ prior to 0.4.0.0.
--
-- @since 0.4.0.0
runResourceInIO
    :: ∀ r a
     . Member (Lift IO) r
    => (∀ x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is likely
       -- some combination of 'runM' and other interpreters composed via '.@'.
    -> Sem (Resource ': r) a
    -> Sem r a
runResourceInIO finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it :: Sem (Resource ': r) x -> IO x
        run_it = finish .@ runResourceInIO_b

    sendM $ X.bracket (run_it a) (run_it . d) (run_it . u)

  BracketOnError alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it :: Sem (Resource ': r) x -> IO x
        run_it = finish .@ runResourceInIO_b

    sendM $ X.bracketOnError (run_it a) (run_it . d) (run_it . u)


------------------------------------------------------------------------------
-- | Run a 'Resource' effect purely.
--
-- @since 0.4.0.0
runResource
    :: ∀ r a
     . Sem (Resource ': r) a
    -> Sem r a
runResource = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it = raise . runResource_b
    resource <- run_it a
    result <- run_it $ u resource
    _ <- run_it $ d resource
    pure result

  BracketOnError alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it = raise . runResource_b

    resource <- run_it a
    result <- run_it $ u resource

    ins <- getInspectorT
    case inspect ins result of
      Just _ -> pure result
      Nothing -> do
        _ <- run_it $ d resource
        pure result
{-# INLINE runResource #-}


runResource_b
    :: ∀ r a
     . Sem (Resource ': r) a
    -> Sem r a
runResource_b = runResource
{-# NOINLINE runResource_b #-}

runResourceInIO_b
    :: ∀ r a
     . Member (Lift IO) r
    => (∀ x. Sem r x -> IO x)
    -> Sem (Resource ': r) a
    -> Sem r a
runResourceInIO_b = runResourceInIO
{-# NOINLINE runResourceInIO_b #-}

