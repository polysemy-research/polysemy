{-# language AllowAmbiguousTypes, BangPatterns #-}

-- | Description: Interpreters for 'Scoped'
module Polysemy.Scoped (
  -- * Effect
  Scoped,
  Scoped_,
  Scoped1,
  Scoped1_,

  -- * Constructors
  scoped,
  scoped_,
  scoped1,
  scoped1_,
  rescope,
  rescope_,
  rescope1,
  rescope1_,

  -- * Interpreters
  runScoped,
  runScoped_,
  runScoped1,
  runScoped1_,
) where

import Data.Functor.Identity
import Data.Functor.Const
import Polysemy
import Polysemy.Bundle
import Polysemy.Opaque
import Polysemy.Internal
import Polysemy.Internal.Utils
import Polysemy.Internal.Union
import Polysemy.Internal.Scoped
import Polysemy.Internal.HigherOrder
import Unsafe.Coerce

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

runScoped ::
  ∀ effect param modifier r .
  (∀ q x.
   param ->
   Sem (effect ': Opaque q ': r) x ->
   Sem (Opaque q ': r) (modifier x)) ->
  InterpreterFor (Scoped effect param modifier) r
runScoped interp =
  coerceEffs
    @_ @(Scoped1
          (Const2 effect :: () -> Effect)
          (Const param)
          (Const1 modifier)
         ': r)
  >>> runScoped1 \(Const param) (sem :: Sem (ce ': Opaque q ': r) x) ->
    Const1 <$> interp @q param (coerceEffs sem)

runScoped_ ::
  ∀ effect param r .
  (∀ q x.
   param ->
   Sem (effect ': Opaque q ': r) x -> Sem (Opaque q ': r) x) ->
  InterpreterFor (Scoped_ effect param) r
runScoped_ interp = runScoped \p sem -> Identity <$> interp p sem

runScoped1 ::
  ∀ effect param modifier r .
  (∀ k q x.
   param k ->
   Sem (effect k ': Opaque q ': r) x ->
   Sem (Opaque q ': r) (modifier k x)
   ) ->
  InterpreterFor (Scoped1 effect param modifier) r
runScoped1 interp = interpretH \case
  RunInScope1 w _ ->
    errorWithoutStackTrace $ "top level run with depth " ++ show w
  Scoped1 ex (param :: param k) main -> do
    (_ :: TypeParamsH z t sc rPre rPost) <- getTypeParamsH
    runH' @z @t @sc @r @r (main 0)
      -- Scoped ... ': HigherOrder ... ': Bundle '[HigherOrder ...] ': r
      & sink @'[_]
      -- HigherOrder ... ': Scoped ... ': Bundle '[HigherOrder ...] ': r
      & rewrite (Bundle (Here @_ @_ @'[]))
      -- Bundle '[HigherOrder ...] ': Scoped ... ': Bundle '[HigherOrder ...] ': r
      & subsumeUsing (There Here)
      -- Scoped ... ': Bundle '[HigherOrder ...] ': r
      & go @k 0
      -- effect k ': OuterRun effect ': Bundle '[HigherOrder ...] ': r
      & subsume_
      -- OuterRun effect ': Bundle '[HigherOrder ...] ': effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
      & consBundle @_ @'[_]
      -- Bundle '[OuterRun effect, HigherOrder ...] ': effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
      & subsumeUsing (There Here)
      -- effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
      & bundleToOpaque
      -- effect k ': Opaque (Bundle '[OuterRun effect, HigherOrder ...]) ': r
      & interp param
      -- Opaque (Bundle '[OuterRun effect, HigherOrder ...]) ': r
      & fromOpaque
      -- Bundle '[OuterRun effect, HigherOrder ...] ': r
      & runBundle
      -- OuterRun effect ': HigherOrder ... ': r
      & interpretH @(OuterRun effect) \case
        OuterRun w _ -> errorWithoutStackTrace $ "unhandled OuterRun with depth " ++ show w
      -- OuterRun effect ': HigherOrder ... ': r
    <&> ex
  where
    sink :: forall mid l r' x before after
          . (before ~ l ': (Append mid r'), after ~ Append mid (l ': r'),
             Subsume before after
            )
         => Sem before x -> Sem after x
    sink = subsume_

    bundleToOpaque :: forall e l x
                    . Sem (e ': Bundle l ': r) x
                   -> Sem (e ': Opaque (Bundle l) ': r) x
    bundleToOpaque = coerceEffs

    go :: forall k l x
        . Word
       -> Sem (Scoped1 effect param modifier ': Bundle l ': r) x
       -> Sem (effect k ': OuterRun effect ': Bundle l ': r) x
    go depth = reinterpret2H \case
        RunInScope1 w act
          | w == depth -> propagateUsing Here (unsafeCoerce act)
          | otherwise -> propagate (OuterRun w act)
        Scoped1 ex (param :: param k') main -> do
          let depth' = depth + 1
          (_ :: TypeParamsH z t sc rPre rPost) <- getTypeParamsH
          runH' @z @t @sc @rPre @rPost (main depth')
            -- Scoped ... ': HigherOrder ... ': Bundle l ': r
            & raiseUnder3
            -- Scoped ... ': HigherOrder ... ': Bundle l ': Bundle (HigherOrder ... ': l) ': r
            & sink @'[_, _]
            -- HigherOrder ... ': Bundle l ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
            & consBundle @_ @l
            -- Bundle (HigherOrder ... ': l) ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
            & subsumeUsing (There Here)
            -- Scoped ... ': Bundle (HigherOrder ... ': l) ': r
            & go @k' depth'
            -- effect k' ': OuterRun effect ': Bundle (HigherOrder ... ': l') ': r
            & subsume_
            -- OuterRun effect ': Bundle (HigherOrder ... ': l') ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            & consBundle @(OuterRun effect) @(_ ': l)
            -- Bundle (OuterRun effect ': HigherOrder ... ': l) ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            & subsumeUsing (There Here)
            -- effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            & bundleToOpaque
            -- effect k' ': Opaque (Bundle (OuterRun effect ': HigherOrder ... ': l)) ': r
            & interp param
            -- Opaque (Bundle (OuterRun effect ': HigherOrder ... ': l)) ': r
            & fromOpaque
            -- Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            & unconsBundle
            -- OuterRun effect ': Bundle (HigherOrder ... ': l) ': r
            & insertAt @2
            -- OuterRun effect ': Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
            & interpretH \case
              orun@(OuterRun w act)
                | w == depth -> propagateUsing (There (There Here)) (unsafeCoerce act)
                | otherwise -> propagate orun
            -- Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
            & unconsBundle
            -- HigherOrder ... ': Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
            & subsumeUsing (There Here)
            -- Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
            & subsumeUsing (There (There (There Here)))
            -- HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
          <&> ex

runScoped1_ ::
  ∀ effect param r .
  (∀ k q x.
   param k ->
   Sem (effect k ': Opaque q ': r) x -> Sem (Opaque q ': r) x
   ) ->
  InterpreterFor (Scoped1_ effect param) r
runScoped1_ interp = runScoped1 \p sem -> (Const1 #. Identity) <$> interp p sem
