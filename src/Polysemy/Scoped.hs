{-# language AllowAmbiguousTypes, BangPatterns #-}

-- | Description: Interpreters for 'Scoped'
module Polysemy.Scoped (
  -- * Effect
  Scoped,
  Scoped_,

  -- * Constructors
  scoped,
  scoped_,
  rescope,

  -- * Interpreters
  runScopedNew,
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

import Data.Function ((&))
import Data.Sequence (Seq(..))
import qualified Data.Sequence as S

import Polysemy.Opaque
import Polysemy.Internal
import Polysemy.Internal.Sing
import Polysemy.Internal.Union
import Polysemy.Internal.Scoped
import Polysemy.Internal.Interpret

-- | Construct an interpreter for a higher-order effect wrapped in a 'Scoped',
-- given a resource allocation function and a parameterized handler for the
-- plain effect.
--
-- This combinator is analogous to 'interpretH' in that it allows the handler to
-- use the 'Polysemy.Handling' environment and transforms the effect into other
-- effects on the stack.
interpretScopedH ::
  ∀ resource param effect r .
  -- | A callback function that allows the user to acquire a resource for each
  -- computation wrapped by 'scoped' using other effects, with an additional
  -- argument that contains the call site parameter passed to 'scoped'.
  (∀ q x . param ->
   (resource -> Sem (Opaque q ': r) x) ->
   Sem (Opaque q ': r) x) ->
  -- | A handler like the one expected by 'interpretH' with an additional
  -- parameter that contains the @resource@ allocated by the first argument.
  (∀ q . resource -> EffHandlerH effect (Opaque q ': r) (Opaque q ': r)) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH withResource scopedHandler = runScopedNew \param sem ->
  withResource param \r -> interpretH (scopedHandler r) sem
{-# inline interpretScopedH #-}

-- | Variant of 'interpretScopedH' that allows the resource acquisition function
-- to use 'Polysemy.Handling'.
interpretScopedH' ::
  ∀ resource param effect r .
  (∀ t e z x . Traversable t =>
    param ->
    (resource -> Sem (Handling z t e r r ': r) x) ->
    Sem (Handling z t e r r ': r) x) ->
  (∀ t z x . Traversable t =>
    resource ->
    effect z x -> Sem (Handling z t (Scoped param effect) r r ': r) x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedH' withResource scopedHandler =
  go 0 Empty
  where
    go :: Word -> Seq resource -> InterpreterFor (Scoped param effect) r
    go depth resources =
      interpretH \case
        Run w act ->
          scopedHandler (S.index resources (fromIntegral w)) act
        InScope param main | !depth' <- depth + 1 -> do
          withResource param \ resource -> controlWithProcessorH $ \prcs ->
            go depth' (resources :|> resource) (prcs (main depth))
{-# inline interpretScopedH' #-}

-- | First-order variant of 'interpretScopedH'.
interpretScoped ::
  ∀ resource param effect r .
  (∀ q x . param ->
   (resource -> Sem (Opaque q ': r) x) ->
   Sem (Opaque q ': r) x) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScoped withResource scopedHandler =
  interpretScopedH withResource \ r e -> (raise . raise) (scopedHandler r e)
{-# inline interpretScoped #-}

-- | Variant of 'interpretScoped' in which the resource allocator is a plain
-- action.
interpretScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (∀ m x . resource -> effect m x -> Sem r x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedAs resource =
  interpretScoped \ p use -> use =<< raise (resource p)
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
  ∀ extra resource param effect r .
  KnownList extra =>
  (∀ q x .
   param ->
   (resource -> Sem (Append extra (Opaque q ': r)) x) ->
   Sem (Opaque q ': r) x) ->
  (∀ q r' .
   r' ~ Append extra (Opaque q ': r) =>
   resource ->
   EffHandlerH effect r' r') ->
  InterpreterFor (Scoped param effect) r
interpretScopedWithH withResource scopedHandler = runScopedNew
  \param (sem :: Sem (effect ': Opaque q ': r) x) ->
    withResource param \resource ->
      sem
        & restack
           (injectMembership (singList @'[effect]) (singList @extra))
        & interpretH (scopedHandler @q resource)
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
  ∀ extra param resource effect r.
  KnownList extra =>
  (∀ q x .
   param ->
   (resource -> Sem (Append extra (Opaque q ': r)) x) ->
   Sem (Opaque q ': r) x) ->
  (∀ m x . resource -> effect m x -> Sem (Append extra r) x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith withResource scopedHandler = runScopedNew
  \param (sem :: Sem (effect ': Opaque q ': r) x) ->
    withResource param \resource ->
      sem
        & restack
           (injectMembership (singList @'[effect]) (singList @extra))
        & interpretH \e -> raise $
            restack
              (injectMembership @r (singList @extra) (singList @'[Opaque q]))
              (scopedHandler resource e)
{-# inline interpretScopedWith #-}

-- | Variant of 'interpretScopedWith' in which no resource is used and the
-- resource allocator is a plain interpreter.
-- This is useful for scopes that only need local effects, but no resources in
-- the handler.
--
-- See the /Note/ on 'interpretScopedWithH'.
interpretScopedWith_ ::
  ∀ extra param effect r .
  KnownList extra =>
  (∀ q x .
   param ->
   Sem (Append extra (Opaque q ': r)) x ->
   Sem (Opaque q ': r) x) ->
  (∀ m x . effect m x -> Sem (Append extra r) x) ->
  InterpreterFor (Scoped param effect) r
interpretScopedWith_ withResource scopedHandler =
  interpretScopedWith @extra
    (\ p f -> withResource p (f ()))
    (\ () -> scopedHandler)
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
-- This renders the resource allocator practically redundant; for the moment,
-- the API surrounding 'Scoped' remains the same, but work is in progress to
-- revamp the entire API of 'Scoped'.
runScoped ::
  ∀ resource param effect r .
  (∀ q x . param -> (resource -> Sem (Opaque q ': r) x) -> Sem (Opaque q ': r) x) ->
  (∀ q . resource -> InterpreterFor effect (Opaque q ': r)) ->
  InterpreterFor (Scoped param effect) r
runScoped withResource scopedInterpreter = runScopedNew \param sem ->
  withResource param (\r -> scopedInterpreter r sem)
{-# inline runScoped #-}

-- | Variant of 'runScoped' in which the resource allocator returns the resource
-- rather than calling a continuation.
runScopedAs ::
  ∀ resource param effect r .
  (param -> Sem r resource) ->
  (∀ q. resource -> InterpreterFor effect (Opaque q ': r)) ->
  InterpreterFor (Scoped param effect) r
runScopedAs resource = runScoped \ p use -> use =<< raise (resource p)
{-# inline runScopedAs #-}

-- | Run a 'Scoped' effect by specifying the interpreter to be used at every
-- use of 'scoped'.
--
-- This interpretation of 'Scoped' is powerful enough to subsume all other
-- interpretations of 'Scoped' (except 'interpretScopedH'' which works
-- differently from all other interpretations) while also being much simpler.
--
-- Consider this a sneak-peek of the future of 'Scoped'. In the API rework
-- planned for 'Scoped', the effect and its interpreters will be further
-- expanded to make 'Scoped' even more flexible.
--
-- @since 1.9.0.0
runScopedNew ::
  ∀ param effect r .
  (∀ q. param -> InterpreterFor effect (Opaque q ': r)) ->
  InterpreterFor (Scoped param effect) r
runScopedNew h = interpretH \case
  Run w _ -> errorWithoutStackTrace $ "top level run with depth " ++ show w
  InScope param main -> controlWithProcessorH $ \prcs ->
    prcs (main 0)
      & raise2Under
      & go 0
      & h param
      & interpretH (\(Opaque (OuterRun w _)) ->
          errorWithoutStackTrace $ "unhandled OuterRun with depth " ++ show w)
  where
    go' :: Word
        -> InterpreterFor
             (Opaque (OuterRun effect))
             (effect ': Opaque (OuterRun effect) ': r)
    go' depth = interpretH \case
      sr@(Opaque (OuterRun w act))
        | w == depth -> propagate act
        | otherwise -> propagate sr

    -- TODO investigate whether loopbreaker optimization is effective here
    go :: Word
       -> InterpreterFor
            (Scoped param effect)
            (effect ': Opaque (OuterRun effect) ': r)
    go depth = interpretH \case
        Run w act
          | w == depth -> propagate act
          | otherwise -> propagate (Opaque (OuterRun w act))
        InScope param main -> controlWithProcessorH $ \prcs -> do
          let !depth' = depth + 1
          prcs (main depth')
            & go depth'
            & h param
            & raise2Under
            & go' depth
{-# INLINE runScopedNew #-}
