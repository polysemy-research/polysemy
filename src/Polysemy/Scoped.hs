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
  runScoped,
  runScopedAs,
) where

import Polysemy.Internal (InterpreterFor, Sem, liftSem, raise)
import Polysemy.Internal.Combinators (interpretH)
import Polysemy.Internal.Scoped (Scoped (InScope, Run), interpretWeaving, rescope, scoped, scoped_)
import Polysemy.Internal.Tactics (Tactical, runTactics, runTSimple, liftT)
import Polysemy.Internal.Union (Weaving (Weaving), injWeaving)

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

-- |Interpreter for 'Scoped', taking a @resource@ allocation function and a parameterized interpreter for the plain
-- @effect@.
--
-- >>> runScoped withResource scopedInterpreter
--
-- @withResource@ is a callback function, allowing the user to acquire the resource for each program from other effects.
--
-- @scopedInterpreter@ is a regular interpreter that is called with the @resource@ argument produced by @scope@.
-- This is mostly useful if you want to reuse an interpreter that you cannot easily rewrite (like from another library).
-- If you have full control over the implementation, 'interpreterScoped' should be preferred.
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
