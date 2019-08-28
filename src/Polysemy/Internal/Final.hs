{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Internal.Final where

import Polysemy
import Polysemy.Internal

-----------------------------------------------------------------------------
-- | This represents a function which produces
-- an action of the final monad @m@ given:
--
--   * The initial effectful state at the moment the action
--     is to be executed.
--
--   * A way to convert @z@ (which is typically @'Sem' r@)to @m@ by
--     threading the effectful state through.
--
--   * An inspector that is able to view some value within the
--     effectful state if the effectful state contains any values.
--
-- A @'Polysemy.Internal.Union.Weaving'@ provides these components,
-- hence the name 'ThroughWeavingToFinal'.
type ThroughWeavingToFinal m z a =
     forall f
   . Functor f
  => f ()
  -> (forall x. f (z x) -> m (f x))
  -> (forall x. f x -> Maybe x)
  -> m (f a)

-----------------------------------------------------------------------------
-- | An effect for embedding higher-order actions in the final target monad
-- of the effect stack.
--
-- This is very useful for writing interpreters that interpret higher-order
-- effects in terms of the final monad.
--
-- 'Final' is slightly weaker, but using it is otherwise more preferable
-- than using a provided lowering function (a la 'Polysemy.Async.lowerAsync'):
--
--   * Interpreters using 'Final' may be run and composed like any other
--     interpreter, unlike @lower-@ interpreters which need to be
--     composed using '.@' or '.@@'.
--
--   * Multiple return-value changing interpreters using 'Final' may be
--     composed together without issue. It's very difficult to
--     compose 'Polysemy.Error.lowerError' with itself, unlike
--     'Polysemy.Error.errorToIOFinal'.
--
--   * Initialization work of @lower-@ interpreters may be duplicated
--     when composed together with '.@'. @-'Final'@ interpreters avoid
--     this issue altogether, as long as the initialization work is performed
--     outside of a 'interpretH' / 'Polysemy.Final.interpretFinal'.
--
--   * Instead of having access to a natural transformation
--     @forall x. 'Sem' r x -> m x@, @'Final' m@ provides a
--     distribution through the effectful state:
--     @forall x. f ('Sem' r x) -> m (f x)@,
--     together with the initial state @f ()@, and an @'Inspector' f@.
--     (See 'ThroughWeavingToFinal')
--     This is powerful enough for most purposes.
--
-- /Beware/: 'Final' actions are interpreted as actions of the final monad,
-- and the effectful state visible to
-- 'withWeavingToFinal' \/ 'Polysemy.Final.withStrategicToFinal'
-- \/ 'Polysemy.Final.interpretFinal'
-- is that of /all interpreters run in order to produce the final monad/.
--
-- This means that any interpreter built using 'Final' will /not/
-- respect local/global state semantics based on the order of
-- interpreters run. You should signal interpreters that make use of
-- 'Final' by adding a @-'Final'@ suffix to the names of these.
--
-- State semantics of effects that are /not/
-- interpreted in terms of the final monad will always
-- appear local to effects that are interpreted in terms of the final monad.
--
-- State semantics between effects that are interpreted in terms of the final monad
-- depend on the final monad. For example, if the final monad is a monad transformer
-- stack, then state semantics will depend on the order monad transformers are stacked.
newtype Final m z a where
  WithWeavingToFinal
    :: ThroughWeavingToFinal m z a
    -> Final m z a

makeSem_ ''Final

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad.
--
-- Consider using 'Polysemy.Final.withStrategicToFinal' instead,
-- which provides a more user-friendly interface, but is also slightly weaker.
--
-- You are discouraged from using 'withWeavingToFinal' directly
-- in application code, as it ties your application code directly to
-- the final monad.
withWeavingToFinal
  :: forall m a r
   . Member (Final m) r
  => ThroughWeavingToFinal m (Sem r) a
  -> Sem r a

data Strategy m f n z a where
  GetInitialState     :: Strategy m f n z (f ())
  HoistInterpretation :: (a -> n b) -> Strategy m f n z (f a -> m (f b))
  GetInspector        :: Strategy m f n z (Inspector f)

------------------------------------------------------------------------------
-- | 'Strategic' is an environment in which you're capable of explicitly
-- threading higher-order effect states to the final monad.
-- This is a variant of @Tactics@ (see 'Tactical'), and usage
-- is extremely similar.
type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (m (f a))

type WithStrategy m f n = '[Strategy m f n]

------------------------------------------------------------------------------
-- | Internal function to process Strategies in terms of 'withWeavingToFinal'.
runStrategy :: Functor f
            => f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> Sem '[Strategy m f n] a
            -> a
runStrategy s wv ins = (run .) $ interpret $ \case
  GetInitialState       -> pure s
  HoistInterpretation f -> pure $ \fa -> wv (f <$> fa)
  GetInspector          -> pure (Inspector ins)
{-# INLINE runStrategy #-}

------------------------------------------------------------------------------
-- | Get a natural transformation capable of potentially inspecting values
-- inside of @f@. Binding the result of 'getInspectorS' produces a function that
-- can sometimes peek inside values returned by 'bindS'.
--
-- This is often useful for running callback functions that are not managed by
-- polysemy code.
--
-- See also 'getInspectorT'
getInspectorS :: forall m f n. Sem (WithStrategy m f n) (Inspector f)
getInspectorS = send (GetInspector @m @f @n)
{-# INLINE getInspectorS #-}

------------------------------------------------------------------------------
-- | Get the stateful environment of the world at the moment the
-- target monad is to be run.
-- Prefer 'pureS', 'liftS', 'runS', or 'bindS' instead of using this function
-- directly.
getInitialStateS :: forall m f n. Sem (WithStrategy m f n) (f ())
getInitialStateS = send (GetInitialState @m @f @n)
{-# INLINE getInitialStateS #-}

------------------------------------------------------------------------------
-- | Embed a value into 'Strategic'.
pureS :: Applicative m => a -> Strategic m n a
pureS a = pure . (a <$) <$> getInitialStateS
{-# INLINE pureS #-}

------------------------------------------------------------------------------
-- | Lifts an action of the final monad into 'Strategic'.
--
-- Note: you don't need to use this function if you already have a monadic
-- action with the functorial state woven into it, by the use of
-- 'runS' or 'bindS'.
-- In these cases, you need only use 'pure' to embed the action into the
-- 'Strategic' environment.
liftS :: Functor m => m a -> Strategic m n a
liftS m = do
  s <- getInitialStateS
  pure $ fmap (<$ s) m
{-# INLINE liftS #-}

------------------------------------------------------------------------------
-- | Lifts a monadic action into the stateful environment, in terms
-- of the final monad.
-- The stateful environment will be the same as the one that the target monad
-- is initially run in.
-- Use 'bindS'  if you'd prefer to explicitly manage your stateful environment.
runS :: n a -> Sem (WithStrategy m f n) (m (f a))
runS na = bindS (const na) <*> getInitialStateS
{-# INLINE runS #-}

------------------------------------------------------------------------------
-- | Embed a kleisli action into the stateful environment, in terms of the final
-- monad. You can use 'bindS' to get an effect parameter of the form @a -> n b@
-- into something that can be used after calling 'runS' on an effect parameter
-- @n a@.
bindS :: (a -> n b) -> Sem (WithStrategy m f n) (f a -> m (f b))
bindS = send . HoistInterpretation
{-# INLINE bindS #-}
