{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Resource
  ( -- * Effect
    Resource (..)

    -- * Actions
  , bracket
  , bracket_
  , bracketOnError
  , finally
  , onException

    -- * Interpretations
  , runResource
  , resourceToIOFinal
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
-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
--
-- cf. 'Control.Exception.bracket' and 'Control.Exception.bracket_'
--
-- @since 1.5.0.0
bracket_
    :: Member Resource r
    => Sem r a -- ^ computation to run first
    -> Sem r b -- ^ computation to run last (even if an exception was raised)
    -> Sem r c -- ^ computation to run in-between
    -> Sem r c
bracket_ begin end act = bracket begin (const end) (const act)

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
finally act end = bracket (pure ()) (const end) (const act)


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

