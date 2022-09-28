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

import Polysemy.Internal (InterpreterFor, Sem, liftSem, raise, Append, insertAt)
import Polysemy.Internal.Combinators (interpretH)
import Polysemy.Internal.Scoped (Scoped (InScope, Run), interpretWeaving, rescope, scoped, scoped_)
import Polysemy.Internal.Tactics (Tactical, runTactics, runTSimple, liftT)
import Polysemy.Internal.Union (Weaving (Weaving), injWeaving)
import Polysemy.Internal.Index (InsertAtIndex)

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized handler for the plain
-- @effect@.
--
-- >>> interpretScopedH withResource scopedHandler
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program wrapped by 'scoped'
-- using other effects, with an additional argument that contains the call site parameter passed to 'scoped'.
--
-- @scopedHandler@ is a handler like the one expected by 'interpretH' with an additional parameter that contains the
-- @resource@ allocated by @withResource@.
interpretScopedH ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH withResource scopedHandler =
  run
  where
    run :: InterpreterFor (Scoped param resource effect) r
    run =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          ex <$> runTactics s (raise . run . wv) ins (run . wv) (scopedHandler resource act)
        InScope param main ->
          withResource param \ resource -> ex <$> run (wv (main resource <$ s))
{-# inline interpretScopedH #-}

-- |Variant of 'interpretScopedH' that allows the resource acquisition function to use 'Tactical'.
interpretScopedH' ::
  ∀ param resource effect r .
  (∀ e m x . param -> (resource -> Tactical e m r x) -> Tactical e m r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical (Scoped param resource effect) (Sem r0) r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedH' withResource scopedHandler =
  interpretH \case
    Run resource act ->
      scopedHandler resource act
    InScope param main ->
      withResource param \ resource ->
        runTSimple (main resource)
{-# inline interpretScopedH' #-}

-- |First-order variant of 'interpretScopedH'.
interpretScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScoped #-}

-- |Variant of 'interpretScoped' in which the resource allocator is a plain action.
interpretScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Sem r x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedAs resource =
  interpretScoped \ p use -> use =<< resource p
{-# inline interpretScopedAs #-}

-- |Higher-order interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the
-- resource allocator.
--
-- /Note/: It is necessary to specify the list of local interpreters with a type application; GHC won't be able to
-- figure them out from the type of @withResource@:
--
-- > interpretScopedWithH @[AtomicState Int, Reader Bool] withResource \ _ -> \case
-- >   SomeAction -> atomicPut . (> 0) =<< ask
interpretScopedWithH ::
  ∀ extra param resource effect r r1 .
  r1 ~ (Append extra r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect] r1 r (Scoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ r0 x . resource -> effect (Sem r0) x -> Tactical effect (Sem r0) r1 x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedWithH withResource scopedHandler =
  interpretWeaving \case
    Weaving (InScope param main) s wv ex _ ->
      ex <$> withResource param \ resource -> inScope (insertAt @1 @extra (wv (main resource <$ s)))
    _ ->
      error "top level Run"
  where
    inScope :: InterpreterFor (Scoped param resource effect) r1
    inScope =
      interpretWeaving \case
        Weaving (InScope param main) s wv ex _ ->
          insertAt @0 @extra (ex <$> withResource param \ resource -> inScope (wv (main resource <$ s)))
        Weaving (Run resource act) s wv ex ins ->
          ex <$> runTactics s (raise . inScope . wv) ins (inScope . wv) (scopedHandler resource act)
{-# inline interpretScopedWithH #-}

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith ::
  ∀ extra param resource effect r r1 .
  r1 ~ (Append extra r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param resource effect] r1 r (Scoped param resource effect : r1) extra =>
  (∀ x . param -> (resource -> Sem r1 x) -> Sem r x) ->
  (∀ m x . resource -> effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param resource effect) r
interpretScopedWith withResource scopedHandler =
  interpretScopedWithH @extra withResource \ r e -> liftT (scopedHandler r e)
{-# inline interpretScopedWith #-}

-- |Interpreter for 'Scoped' that allows the handler to use additional effects that are interpreted by the resource
-- allocator.
--
-- In this variant, no resource is used and the allocator is a plain interpreter.
-- This is useful for scopes that only need local effects, but no resources in the handler.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith_ ::
  ∀ extra param effect r r1 .
  r1 ~ (Append extra r) =>
  InsertAtIndex 0 '[] r1 r r1 extra =>
  InsertAtIndex 1 '[Scoped param () effect] r1 r (Scoped param () effect : r1) extra =>
  (∀ x . param -> Sem r1 x -> Sem r x) ->
  (∀ m x . effect m x -> Sem r1 x) ->
  InterpreterFor (Scoped param () effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWithH @extra (\ p f -> withResource p (f ())) \ () e -> liftT (scopedHandler e)
{-# inline interpretScopedWith_ #-}

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- >>> runScoped withResource scopedInterpreter
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- This is mostly useful if you want to reuse an interpreter that you cannot easily rewrite (like from another library).
-- If you have full control over the implementation, 'interpreteScoped' should be preferred.
--
-- /Note/: This function will be called for each action in the program, so if the interpreter allocates any resources,
-- they will be scoped to a single action. Move them to @withResource@ instead.
runScoped ::
  ∀ param resource effect r .
  (∀ x . param -> (resource -> Sem r x) -> Sem r x) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScoped withResource scopedInterpreter =
  run
  where
    run :: InterpreterFor (Scoped param resource effect) r
    run =
      interpretWeaving \ (Weaving effect s wv ex ins) -> case effect of
        Run resource act ->
          scopedInterpreter resource (liftSem $ injWeaving $ Weaving act s (raise . run . wv) ex ins)
        InScope param main ->
          withResource param \ resource -> ex <$> run (wv (main resource <$ s))
{-# inline runScoped #-}

-- |Variant of 'runScoped' in which the resource allocator is a plain action.
runScopedAs ::
  ∀ param resource effect r .
  (param -> Sem r resource) ->
  (resource -> InterpreterFor effect r) ->
  InterpreterFor (Scoped param resource effect) r
runScopedAs resource =
  runScoped \ p use -> use =<< resource p
{-# inline runScopedAs #-}
