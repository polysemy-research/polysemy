{-# LANGUAGE TemplateHaskell, DeriveTraversable #-}

-- | Description: The 'Bracket' effect, providing bracketing functionality
module Polysemy.Bracket
  ( -- * Effect
    Bracket (..)

    -- * Actions
  , ExitCase(..)
  , generalBracket
  , bracket
  , bracket_
  , bracketOnError
  , finally
  , onException

    -- * Interpretations
  , runBracketLocally
  , bracketToIOFinal
  ) where

import qualified Control.Exception as X
import           Control.Monad
import           Polysemy
import           Polysemy.Final
import           Polysemy.HigherOrder

------------------------------------------------------------------------------
-- | An effect capable of providing 'X.bracket' semantics. Interpreters for this
-- will successfully run the deallocation action even in the presence of other
-- short-circuiting effects.
data Bracket :: Effect where
  -- | Allocate a resource, use it, and clean it up afterwards.
  GeneralBracket
    :: m a
       -- Action to allocate a resource.
    -> (a -> ExitCase b -> m c)
       -- Action to cleanup the resource. This is guaranteed to be
       -- called.
    -> (a -> m b)
       -- Action which uses the resource.
    -> Bracket m (b, c)


-- | A copy of @ExitCase@ from the @exceptions@ package, with additional
-- 'Functor', 'Foldable', 'Traversable', 'Applicative', and 'Monad' instances.
--
-- A bracketed computation may either succeed with a value, abort with an
-- exception, or abort for some other reason.
data ExitCase a
  = ExitCaseSuccess a
  | ExitCaseException X.SomeException
  | ExitCaseAbort
  deriving (Show, Functor, Foldable, Traversable)

instance Applicative ExitCase where
  pure = ExitCaseSuccess
  (<*>) = ap

instance Monad ExitCase where
  ExitCaseSuccess a >>= f = f a
  ExitCaseException e >>= _ = ExitCaseException e
  ExitCaseAbort >>= _ = ExitCaseAbort

makeSem_ ''Bracket

-- | A generalized version of 'bracket' which uses 'ExitCase' to distinguish
-- the different exit cases, and returns the values of both the 'use' and
-- 'release' actions. In practice, this extra information is rarely needed,
-- so it is often more convenient to use one of the simpler functions which
-- are defined in terms of this one, such as 'bracket', 'finally', 'onError',
-- and 'bracketOnError'.
--
-- @since 2.0.0.0
generalBracket :: forall r a b c
                . Member Bracket r
               => Sem r a
               -> (a -> ExitCase b -> Sem r c)
               -> (a -> Sem r b)
               -> Sem r (b, c)

-- | Generalized abstracted pattern of safe resource acquisition and release
-- in the face of errors. The first action \"acquires\" some value, which
-- is \"released\" by the second action at the end. The third action \"uses\"
-- the value and its result is the result of the 'bracket'.
--
-- @since 0.1.0.0
bracket :: Member Bracket r
        => Sem r a
        -> (a -> Sem r c)
        -> (a -> Sem r b)
        -> Sem r b
bracket acquire release use =
  fst <$> generalBracket acquire (\a _ -> release a) use
{-# INLINE bracket #-}

------------------------------------------------------------------------------
-- | A variant of 'bracket' where the return value from the first computation
-- is not required.
--
-- cf. 'Control.Exception.bracket' and 'Control.Exception.bracket_'
--
-- @since 1.5.0.0
bracket_
    :: Member Bracket r
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
    :: Member Bracket r
    => Sem r a -- ^ computation to run first
    -> Sem r b -- ^ computation to run afterward (even if an exception was raised)
    -> Sem r a
finally act end = bracket (pure ()) (const end) (const act)

-- @since 0.4.0.0
bracketOnError :: Member Bracket r
               => Sem r a
               -> (a -> Sem r c)
               -> (a -> Sem r b)
               -> Sem r b
bracketOnError acquire release use =
  fst <$>
    generalBracket
    acquire (\a -> \case
                ExitCaseSuccess _ -> return ()
                _ -> void $ release a
            )
    use
{-# INLINE bracketOnError #-}

------------------------------------------------------------------------------
-- | Like 'bracketOnError', but for the simple case of one computation to run
-- afterward.
--
-- @since 0.4.0.0
onException
    :: Member Bracket r
    => Sem r a -- ^ computation to run first
    -> Sem r b -- ^ computation to run afterward if an exception was raised
    -> Sem r a
onException act end = bracketOnError (pure ()) (const end) (const act)

------------------------------------------------------------------------------
-- | Run a 'Bracket' effect in terms of 'X.bracket' through final 'IO'
--
-- /Note/: Effects that aren't interpreted in terms of 'IO' will have local
-- state semantics in regards to 'Bracket' effects interpreted this way.
-- See 'Final'.
--
-- @since 1.2.0.0
bracketToIOFinal :: Member (Final IO) r
                  => Sem (Bracket ': r) a
                  -> Sem r a
bracketToIOFinal = interpretFinal @IO $ \case
  GeneralBracket alloc dealloc use -> do
    let release a ec = withProcessorL $ \lower ->
          X.try @X.SomeException $ X.uninterruptibleMask_ $ lower $ dealloc a ec
    controlL $ \lower -> X.mask $ \restore -> lower $ do
      a <- runL alloc
      etb <- withProcessorL $ \lower' -> X.try $ restore (lower' (use a))
      case etb of
        Left e -> do
          _ <- release a (ExitCaseException e)
          embed (X.throwIO e)
        Right tb | Just tVoid <- traverse (const Nothing) tb -> do
          _ <- release a ExitCaseAbort
          restoreL tVoid
        Right tb -> do
          b <- restoreL tb
          c <-     release a (ExitCaseSuccess b)
               >>= either (embed . X.throwIO) return
               >>= restoreL
          return (b, c)
{-# INLINE bracketToIOFinal #-}


------------------------------------------------------------------------------
-- | Run a 'Bracket' effect purely, which protects against failures of all
-- effects already interpreted, if they were interpreted purely -- that is,
-- not in terms of any effects in @r@ or the final monad. In other words,
-- 'runBracketLocally' protects against any failures of local effectful state --
-- hence the name.
--
-- @since 1.0.0.0
runBracketLocally
    :: ∀ r a
     . Sem (Bracket ': r) a
    -> Sem r a
runBracketLocally = interpretH $ \case
  GeneralBracket alloc dealloc use -> do
    a  <- runH alloc
    tb <- runExposeH (use a)
    -- If "use" failed locally -- which we determine by inspecting
    -- the effectful state -- then we run 'dealloc', discarding any changes
    -- it does to the local state.
    case traverse (const Nothing) tb of
      Just tVoid -> do
        _ <- runExposeH $ dealloc a ExitCaseAbort
        restoreH tVoid
      Nothing -> do
        -- If "use" succeeded, then the effectful state is restored and dealloc
        -- is run as normal.
        b <- restoreH tb
        c <- runH $ dealloc a (ExitCaseSuccess b)
        return (b, c)
{-# INLINE runBracketLocally #-}
