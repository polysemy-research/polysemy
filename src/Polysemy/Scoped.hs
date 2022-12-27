{-# language AllowAmbiguousTypes #-}

module Polysemy.Scoped (
  -- * Effect
  Scoped,
  Scoped_,

  -- * Constructors
  scoped,
  scoped_,
  rescope,

  -- * Interpreters
  interpretScopedH,
  interpretScopedH',
  interpretScoped,
  interpretScopedAs,
  interpretScopedWithH,
  interpretScopedWith,
  interpretScopedWith_,
  runScoped,
  runScopedAs,
) where

import Polysemy.Internal
import Polysemy.Internal.Sing
import Polysemy.Internal.Union
import Polysemy.Internal.Combinators
import Polysemy.Internal.Scoped
import Polysemy.Internal.Tactics

-- | Construct an interpreter for a higher-order effect wrapped in a 'Scoped',
-- given a resource allocation function and a parameterized handler for the
-- plain effect.
--
-- This combinator is analogous to 'interpretH' in that it allows the handler to
-- use the 'Tactical' environment and transforms the effect into other effects
-- on the stack.
interpretScopedH ::
  ∀ resource param effect r .
  -- | A callback function that allows the user to acquire a resource for each
  -- computation wrapped by 'scoped' using other effects, with an additional
  -- argument that contains the call site parameter passed to 'scoped'.
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  -- | A handler like the one expected by 'interpretH' with an additional
  -- parameter that contains the @resource@ allocated by the first argument.
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH withResource scopedHandler =
  interpretWeaving $ \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
          ex
      <$> interpretH (scopedHandler resource) (go $ raiseUnder $ wv (main <$ s))
  where
    -- TODO investigate whether loopbreaker optimization is effective here
    go :: InterpreterFor (Scoped param effect) (effect ': r)
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (go . wv) ex ins
        InScope param main -> raise $ withResource param \ resource ->
          ex <$> interpretH (scopedHandler resource) (go $ wv (main <$ s))
{-# inline interpretScopedH #-}

-- | Variant of 'interpretScopedH' that allows the resource acquisition function
-- to use 'Tactical'.
interpretScopedH' ::
  ∀ resource param effect r .
  (∀ e r0 x . param -> (resource -> Tactical e (Sem r0) r x) ->
    Tactical e (Sem r0) r x) ->
  (∀ r0 x .
    resource -> effect (Sem r0) x ->
    Tactical (Scoped param effect) (Sem r0) r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH' withResource scopedHandler =
  go (errorWithoutStackTrace "top level run")
  where
    go :: resource -> InterpreterFor (Scoped param effect) r
    go resource =
      interpretH \case
        Run act ->
          scopedHandler resource act
        InScope param main ->
          withResource param \ resource' ->
            raise . go resource' =<< runT main
{-# inline interpretScopedH' #-}

-- | First-order variant of 'interpretScopedH'.
interpretScoped ::
  ∀ resource param effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScoped #-}

-- | Variant of 'interpretScoped' in which the resource allocator is a plain
-- action.
interpretScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedAs resource =
  interpretScoped \ p use -> use =<< resource p
{-# inline interpretScopedAs #-}

-- | Higher-order interpreter for 'Scoped' that allows the handler to use
-- additional effects that are interpreted by the resource allocator.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type
-- application; GHC won't be able to figure them out from the type of
-- @withResource@.
--
-- As an example for a higher order effect, consider a mutexed concurrent state
-- effect, where an effectful function may lock write access to the state while
-- making it still possible to read it:
--
-- > data MState s :: Effect where
-- >   MState :: (s -> m (s, a)) -> MState s m a
-- >   MRead :: MState s m s
-- >
-- > makeSem ''MState
--
-- We can now use an 'Polysemy.AtomicState.AtomicState' to store the current
-- value and lock write access with an @MVar@. Since the state callback is
-- effectful, we need a higher order interpreter:
--
-- > withResource ::
-- >   Member (Embed IO) r =>
-- >   s ->
-- >   (MVar () -> Sem (AtomicState s : r) a) ->
-- >   Sem r a
-- > withResource initial use = do
-- >   tv <- embed (newTVarIO initial)
-- >   lock <- embed (newMVar ())
-- >   runAtomicStateTVar tv $ use lock
-- >
-- > interpretMState ::
-- >   ∀ s r .
-- >   Members [Resource, Embed IO] r =>
-- >   InterpreterFor (Scoped s (MState s)) r
-- > interpretMState =
-- >   interpretScopedWithH @'[AtomicState s] withResource \ lock -> \case
-- >     MState f ->
-- >       bracket_ (embed (takeMVar lock)) (embed (tryPutMVar lock ())) do
-- >         s0 <- atomicGet
-- >         res <- runTSimple (f s0)
-- >         Inspector ins <- getInspectorT
-- >         for_ (ins res) \ (s, _) -> atomicPut s
-- >         pure (snd <$> res)
-- >     MRead ->
-- >       liftT atomicGet
interpretScopedWithH ::
  ∀ extra resource param effect r r1 .
  (KnownList extra, r1 ~ Append extra r) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource ->
        interpretH (scopedHandler resource) $ inScope $
          restack
            (injectMembership
             (singList @'[Scoped param effect])
             (singList @(effect ': extra))) $ wv (main <$ s)
    _ ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param effect) (effect ': r1)
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          restack
            (extendMembershipLeft (singList @(effect ': extra)))
            (ex <$> withResource param \resource ->
                interpretH (scopedHandler resource) $ inScope $ wv (main <$ s))
        Weaving (Run act) s wv ex ins ->
          liftSem $ injWeaving $ Weaving act s (inScope . wv) ex ins
{-# inline interpretScopedWithH #-}

-- | First-order variant of 'interpretScopedWithH'.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type
-- application; GHC won't be able to figure them out from the type of
-- @withResource@:
--
-- > data SomeAction :: Effect where
-- >   SomeAction :: SomeAction m ()
-- >
-- > foo :: InterpreterFor (Scoped () SomeAction) r
-- > foo =
-- >   interpretScopedWith @[Reader Int, State Bool] localEffects \ () -> \case
-- >     SomeAction -> put . (> 0) =<< ask @Int
-- >   where
-- >     localEffects () use = evalState False (runReader 5 (use ()))
interpretScopedWith ::
  ∀ extra param resource effect r r1 .
  (r1 ~ Append extra r, KnownList extra) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScopedWith #-}

-- | Variant of 'interpretScopedWith' in which no resource is used and the
-- resource allocator is a plain interpreter.
-- This is useful for scopes that only need local effects, but no resources in
-- the handler.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith_ ::
  ∀ extra param effect r r1 .
  (r1 ~ Append extra r, KnownList extra) =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)
{-# inline interpretScopedWith_ #-}

-- | Variant of 'interpretScoped' that uses another interpreter instead of a
-- handler.
--
-- This is mostly useful if you want to reuse an interpreter that you cannot
-- easily rewrite (like from another library). If you have full control over the
-- implementation, 'interpretScoped' should be preferred.
--
-- /Note/: In previous versions of Polysemy, the wrapped interpreter was
-- executed fully, including the initializing code surrounding its handler,
-- for each action in the program. However, new and continuing discoveries
-- regarding 'Scoped' has allowed the improvement of having the interpreter be
-- used only once per use of 'scoped', and have it cover the same scope of
-- actions that the resource allocator does.
--
-- This renders the resource allocator practically redundant; for the moment, the API
-- surrounding 'Scoped' remains the same, but work is in progress to revamp the
-- entire API of 'Scoped'.
runScoped ::
  ∀ resource param effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param effect) r
runScoped withResource scopedInterpreter =
  interpretWeaving \(Weaving effect s wv ex _) -> case effect of
    Run _ -> errorWithoutStackTrace "top level run"
    InScope param main -> withResource param \ resource ->
      ex <$> scopedInterpreter resource (go (raiseUnder $ wv (main <$ s)))
  where
    go :: InterpreterFor (Scoped param effect) (effect ': r)
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run act -> liftSem $ injWeaving $ Weaving act s (go . wv) ex ins
        InScope param main ->
          raise $ withResource param \ resource ->
            ex <$> scopedInterpreter resource (go (wv (main <$ s)))
{-# inline runScoped #-}

-- | Variant of 'runScoped' in which the resource allocator returns the resource
-- rather tnen calling a continuation.
runScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param effect) r
runScopedAs resource = runScoped \ p use -> use =<< resource p
{-# inline runScopedAs #-}
