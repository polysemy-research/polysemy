{-# language AllowAmbiguousTypes #-}

module Polysemy.Scoped (
  -- * Effect
  Scoped,

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

-- | Interpreter for 'Scoped', taking a @resource@ allocation function and a
-- parameterized handler for the plain @effect@.
--
-- >>> interpretScopedH withResource scopedHandler
--
-- @withResource@ is a callback function, allowing the user to acquire the
-- resource for each program wrapped by 'scoped' using other effects, with an
-- additional argument that contains the call site parameter passed to 'scoped'.
--
-- @scopedHandler@ is a handler like the one expected by 'interpretH' with an
-- additional parameter that contains the @resource@ allocated by
-- @withResource@.
interpretScopedH ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH withResource scopedHandler =
  go
  where
    go :: InterpreterFor (Scoped param resource effect) r
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          ex <$> runTactics s (raise . go . wv) ins (go . wv)
            (scopedHandler resource act)
        InScope param main ->
          withResource param \ resource -> ex <$> go (wv (main resource <$ s))
{-# inline interpretScopedH #-}

-- | Variant of 'interpretScopedH' that allows the resource acquisition function
-- to use 'Tactical'.
interpretScopedH' ::
  ∀ resource param effect r .
  (∀ e r0 x . param -> (resource -> Tactical e (Sem r0) r x) ->
    Tactical e (Sem r0) r x) ->
  (∀ r0 x .
    resource -> effect (Sem r0) x ->
    Tactical (Scoped param resource effect) (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH' withResource scopedHandler =
  interpretH \case
    Run resource act ->
      scopedHandler resource act
    InScope param main ->
      withResource param \ resource ->
        runTSimple (main resource)
{-# inline interpretScopedH' #-}

-- | First-order variant of 'interpretScopedH'.
interpretScoped ::
  ∀ resource param effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScoped #-}

-- | Variant of 'interpretScoped' in which the resource allocator is a plain
-- action.
interpretScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
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
-- >   InterpreterFor (Scoped s (MVar ()) (MState s)) r
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
  InterpreterFor (Scoped param resource effect) r
interpretScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource -> inScope $
        restack
          (injectMembership
           (singList @'[Scoped param resource effect])
           (singList @extra)) $ wv (main resource <$ s)
    Weaving (Run resource act) s wv ex ins ->
      errorWithoutStackTrace "top level Run"
  where
    inScope :: InterpreterFor (Scoped param resource effect) r1
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          restack (extendMembershipLeft (singList @extra))
            (ex <$> withResource param \resource ->
                inScope (wv (main resource <$ s)))
        Weaving (Run resource act) s wv ex ins ->
          ex <$> runTactics s (raise . inScope . wv) ins (inScope . wv)
            (scopedHandler resource act)
{-# inline interpretScopedWithH #-}

-- | Interpreter for 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type
-- application; GHC won't be able to figure them out from the type of
-- @withResource@:
--
-- > interpretScopedWith @[State Bool, Reader Int] withResource \ _ -> \case
-- >   SomeAction -> put . (> 0) =<< ask
-- > where
-- >   withResource param use = runState False (runReader 5 (use ()))
interpretScopedWith ::
  ∀ extra param resource effect r r1 .
  (r1 ~ Append extra r, KnownList extra) =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScopedWith #-}

-- | Interpreter for 'Scoped' that allows the handler to use additional effects
-- that are interpreted by the resource allocator.
--
-- In this variant, no resource is used and the allocator is a plain
-- interpreter. This is useful for scopes that only need local effects, but no
-- resources in the handler.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith_ ::
  ∀ extra param effect r r1 .
  (r1 ~ Append extra r, KnownList extra) =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param () effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)
{-# inline interpretScopedWith_ #-}

-- | Interpreter for 'Scoped', taking a @resource@ allocation function and a
-- parameterized interpreter for the plain @effect@.
-- TODO: look at this documentation again.
--
-- > runScoped withResource scopedInterpreter
--
-- @withResource@ is a callback function, allowing the user to acquire the
-- resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the
-- @resource@ argument produced by @scope@. This is mostly useful if you want to
-- reuse an interpreter that you cannot easily rewrite (like from another
-- library). If you have full control over the implementation,
-- 'interpretScoped' should be preferred.
--
-- /Note/: This function will be called for each action in the program, so if
-- the interpreter allocates any resources, they will be scoped to a single
-- action. Move them to @withResource@ instead.
runScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScoped withResource scopedInterpreter =
  go
  where
    go :: InterpreterFor (Scoped param resource effect) r
    go =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource
            $ liftSem $ injWeaving $ Weaving act s (raise . go . wv) ex ins
        InScope param main ->
          withResource param \ resource -> ex <$> go (wv (main resource <$ s))
{-# inline runScoped #-}

-- | Variant of 'runScoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScopedAs resource = runScoped \ p use -> use =<< resource p
{-# inline runScopedAs #-}
