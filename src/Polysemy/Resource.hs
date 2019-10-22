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
  , resourceToIOFinal
  , resourceToIO
  , lowerResource
  ) where

import qualified Control.Exception as X
import           Polysemy
import           Polysemy.Final


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
-- | Run a 'Resource' effect in terms of 'X.bracket' through final 'IO'
--
-- /Beware/: Effects that aren't interpreted in terms of 'IO'
-- will have local state semantics in regards to 'Resource' effects
-- interpreted this way. See 'Final'.
--
-- Notably, unlike 'resourceToIO', this is not consistent with
-- 'Polysemy.State.State' unless 'Polysemy.State.runStateInIORef' is used.
-- State that seems like it should be threaded globally throughout 'bracket's
-- /will not be./
--
-- Use 'resourceToIO' instead if you need to run
-- pure, stateful interpreters after the interpreter for 'Resource'.
-- (Pure interpreters are interpreters that aren't expressed in terms of
-- another effect or monad; for example, 'Polysemy.State.runState'.)
--
-- @since 1.2.0.0
resourceToIOFinal :: Member (Final IO) r
                  => Sem (Resource ': r) a
                  -> Sem r a
resourceToIOFinal = interpretFinal $ \case
  Bracket alloc dealloc use -> do
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $ X.bracket a d u

  BracketOnError alloc dealloc use -> do
    ins <- getInspectorS
    a <- runS  alloc
    d <- bindS dealloc
    u <- bindS use
    pure $
      X.bracketOnError
        a
        d
        (\x -> do
          result <- u x
          case inspect ins result of
            Just _ -> pure result
            Nothing -> do
              _ <- d x
              pure result
        )

{-# INLINE resourceToIOFinal #-}


------------------------------------------------------------------------------
-- | Run a 'Resource' effect in terms of 'X.bracket'.
--
-- @since 1.0.0.0
lowerResource
    :: ∀ r a
     . Member (Embed IO) r
    => (∀ x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is likely
       -- some combination of 'runM' and other interpreters composed via '.@'.
    -> Sem (Resource ': r) a
    -> Sem r a
lowerResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it :: Sem (Resource ': r) x -> IO x
        run_it = finish .@ lowerResource

    embed $ X.bracket (run_it a) (run_it . d) (run_it . u)

  BracketOnError alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it :: Sem (Resource ': r) x -> IO x
        run_it = finish .@ lowerResource

    embed $ X.bracketOnError (run_it a) (run_it . d) (run_it . u)
{-# INLINE lowerResource #-}
{-# DEPRECATED lowerResource "Use 'resourceToIOFinal' instead" #-}


------------------------------------------------------------------------------
-- | Run a 'Resource' effect purely.
--
-- @since 1.0.0.0
runResource
    :: ∀ r a
     . Sem (Resource ': r) a
    -> Sem r a
runResource = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it = raise . runResource
    resource <- run_it a
    result <- run_it $ u resource
    _ <- run_it $ d resource
    pure result

  BracketOnError alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let run_it = raise . runResource

    resource <- run_it a
    result <- run_it $ u resource

    ins <- getInspectorT
    case inspect ins result of
      Just _ -> pure result
      Nothing -> do
        _ <- run_it $ d resource
        pure result
{-# INLINE runResource #-}


------------------------------------------------------------------------------
-- | A more flexible --- though less safe ---  version of 'resourceToIOFinal'
--
-- This function is capable of running 'Resource' effects anywhere within an
-- effect stack, without relying on an explicit function to lower it into 'IO'.
-- Notably, this means that 'Polysemy.State.State' effects will be consistent
-- in the presence of 'Resource'.
--
-- ResourceToIO' is safe whenever you're concerned about exceptions thrown
-- by effects _already handled_ in your effect stack, or in 'IO' code run
-- directly inside of 'bracket'. It is not safe against exceptions thrown
-- explicitly at the main thread. If this is not safe enough for your use-case,
-- use 'resourceToIOFinal' instead.
--
-- This function creates a thread, and so should be compiled with @-threaded@.
--
-- @since 1.0.0.0
resourceToIO
    :: forall r a
     . Member (Embed IO) r
    => Sem (Resource ': r) a
    -> Sem r a
resourceToIO = interpretH $ \case
  Bracket a b c -> do
    ma <- runT a
    mb <- bindT b
    mc <- bindT c

    withLowerToIO $ \lower finish -> do
      let done :: Sem (Resource ': r) x -> IO x
          done = lower . raise . resourceToIO
      X.bracket
          (done ma)
          (\x -> done (mb x) >> finish)
          (done . mc)

  BracketOnError a b c -> do
    ins <- getInspectorT
    ma <- runT a
    mb <- bindT b
    mc <- bindT c

    withLowerToIO $ \lower finish -> do
      let done :: Sem (Resource ': r) x -> IO x
          done = lower . raise . resourceToIO
      X.bracketOnError
          (done ma)
          (\x -> done (mb x) >> finish)
          (\x -> do
            result <- done $ mc x
            case inspect ins result of
              Just _ -> pure result
              Nothing -> do
                _ <- done $ mb x
                pure result
          )
{-# INLINE resourceToIO #-}

