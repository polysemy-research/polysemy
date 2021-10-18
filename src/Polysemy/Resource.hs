{-# LANGUAGE TemplateHaskell, NondecreasingIndentation #-}

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
  , resourceToIO
  , lowerResource
  ) where

import qualified Control.Exception as X
import           Control.Monad
import           Polysemy
import           Polysemy.Final
import           Polysemy.Interpretation

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
  Bracket alloc dealloc use ->
    controlS' $ \lower -> X.mask $ \restore -> lower $ do
      a <- runS alloc
      tb <- liftWithS $ \lower' ->
              restore (lower' (use a))
            `X.onException`
              lower' (dealloc a)
      case traverse (const Nothing) tb of
        Just tVoid -> do
          _ <- runS (dealloc a)
          restoreS tVoid
        Nothing -> do
          b <- restoreS tb
          _ <- runS (dealloc a)
          return b

  BracketOnError alloc dealloc use ->
    controlS' $ \lower -> X.mask $ \restore -> lower $ do
      a <- runS alloc
      tb <- liftWithS $ \lower' ->
              restore (lower' (use a))
            `X.onException`
              lower' (dealloc a)
      case traverse (const Nothing) tb of
        Just tVoid -> do
          _ <- runS (dealloc a)
          restoreS tVoid
        Nothing -> restoreS tb
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
  Bracket alloc dealloc use -> controlH $ \lower ->
    embed $ X.mask $ \restore -> do
    tr <- finish $ lower $ runH alloc
    case traverse (const Nothing) tr of
      Just tVoid -> return tVoid
      Nothing -> do
        tu <- restore (finish $ lower $ restoreH tr >>= \r -> (,) r <$> runH (use r))
                `X.onException` finish (lower (restoreH tr >>= runH . dealloc))
        case traverse (const Nothing) tu of
          Just tVoid -> tVoid <$ (finish $ lower $ restoreH tr >>= runH . dealloc)
          Nothing -> finish $ lower $ restoreH tu >>= \(r, u) -> u <$ runH (dealloc r)

  BracketOnError alloc dealloc use -> controlH $ \lower ->
    embed $ X.mask $ \restore -> do
    tr <- finish $ lower $ runH $ alloc
    case traverse (const Nothing) tr of
      Just tVoid -> return tVoid
      Nothing -> do
        tu <- restore (finish $ lower $ restoreH tr >>= runH . use)
                `X.onException` finish (lower (restoreH tr >>= runH . dealloc))
        case traverse (const Nothing) tu of
          Just tVoid -> tVoid <$ (finish $ lower $ restoreH tr >>= runH . dealloc)
          Nothing    -> return tu
{-#     INLINE lowerResource #-}
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
    r  <- runH alloc
    ta <- runExposeH (use r)
    -- If "use" failed locally -- which we determine by inspecting
    -- the effectful state -- then we run 'dealloc', discarding any
    -- changes it does to the local state.
    if null ta then do
      _ <- runExposeH (dealloc r)
      restoreH ta
    else do
      -- If "use" succeeded, then the effectful state is restored and dealloc is
      -- run as normal.
      a <- restoreH ta
      _ <- runH (dealloc r)
      return a

  BracketOnError alloc dealloc use -> do
    r  <- runH  alloc
    ta <- runExposeH (use r)
    when (null ta) $ do
      _ <- runExposeH (dealloc r)
      return ()
    restoreH ta
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
resourceToIO =
  undefined
  -- interpretH $ \case
  -- Bracket a b c -> do
  --   ma <- runT a
  --   mb <- bindT b
  --   mc <- bindT c

  --   withLowerToIO $ \lower finish -> do
  --     let done :: Sem (Resource ': r) x -> IO x
  --         done = lower . raise . resourceToIO
  --     X.bracket
  --         (done ma)
  --         (\x -> done (mb x) >> finish)
  --         (done . mc)

  -- BracketOnError a b c -> do
  --   ins <- getInspectorT
  --   ma <- runT a
  --   mb <- bindT b
  --   mc <- bindT c

  --   withLowerToIO $ \lower finish -> do
  --     let done :: Sem (Resource ': r) x -> IO x
  --         done = lower . raise . resourceToIO
  --     X.bracketOnError
  --         (done ma)
  --         (\x -> done (mb x) >> finish)
  --         (\x -> do
  --           result <- done $ mc x
  --           case inspect ins result of
  --             Just _ -> pure result
  --             Nothing -> do
  --               _ <- done $ mb x
  --               pure result
  --         )
{-# INLINE resourceToIO #-}

