{-# language AllowAmbiguousTypes, BangPatterns, GeneralizedNewtypeDeriving #-}

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

  MetaEffect,
  Meta,
  sendMeta,
  genericInterpretMeta,
  interpretMeta,
  MetaHandler,
  runMeta,
  runMeta'
  -- toMeta,
  -- runMeta
) where

import Control.Monad.Trans
import Polysemy.Internal.WeaveClass

import Data.Unique
import Data.Functor.Identity
import Data.Functor.Compose
import Data.Proxy
import Data.Kind
import Data.Functor.Const
import Polysemy
import Polysemy.Fatal
import Polysemy.Final
import Polysemy.Bundle
import Polysemy.Membership
import Polysemy.Opaque
import Polysemy.Internal
import Polysemy.Internal.Utils
import Polysemy.Internal.Union
import Polysemy.Internal.Scoped
import Polysemy.Internal.HigherOrder
import System.IO.Unsafe
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

data WithTail :: EffectRow -> (Effect -> Effect) -> Effect -> Effect where
  WithTail :: z ~ Sem (eff ': Append r r0)
           => param eff z a
           -> WithTail r param eff z a


-- data FreeT f m a where Pure a | FreeT (m (f (FreeT f m a)))

-- liftWithFree :: (Functor f, Monad m) => ((forall x. FreeT f m x -> m (f x)) -> m (f a)) -> FreeT f m a
-- liftWithFree main = FreeT ((fmap . fmap) Pure (main _))
-- runOmegaScopedWithTail ::
--   ∀ extra param r .
--     KnownList extra =>
--   ( ∀ eff q z x
--     . (forall y. z y -> Sem (eff ': Append extra (Opaque q ': r)) y)
--    -> param eff z x -> Sem (Opaque q ': Append extra r) x) ->
--   InterpreterFor (OmegaScoped (WithTail extra param)) r
-- runOmegaScopedWithTail interp = runOmegaScoped \(n :: forall y. z y -> Sem (eff ': Opaque q ': r) y) (WithTail param) ->
--   Sem $ \k -> runSem (interp @eff @q
--                         (mapMembership (injectMembership
--                                          (singList @'[_])
--                                          (singList @extra)) . n) param) $ \case
--     Union Here wav -> _

-- data Sender l :: Effect where
--   Bundle l m a -> Sender rFinal l
-- sick :: forall e r0 t eH rPre rPost a. Traversable t => (forall x. Sem rPost x -> Sem (e ': r0) x) -> Sem (e ': HigherOrder (Sem (e ': r0)) t eH rPre rPost ': rPost) a -> Sem (HigherOrder (Sem (e ': r0)) t eH rPre rPost ': Final (Sem rPost) ': rPost) a
-- sick n = raiseUnder2 >>> interpretH \e -> controlFinal @(Sem rPost) $ \lwr -> do
--   res <- lwr $ raise $ runH $ Sem $ \k -> k $ Union Here $ Weaving e (\n z -> _ $ lwr (runH z)) _ id
--   _
  -- where
    -- gooba :: forall x. Sem (HigherOrder z t e rPre rPost ': rPost) x -> Carrying z t e rPre rPost (Sem r0) x
    -- gooba = _

  -- res <- liftWithH $ \lwr -> liftWithH $ \lwr' -> lowerCarrying lwr $ lowerCarrying lwr' $ getComposeT $ liftHandlerWithNat _ _ $ Union Here $ mkWeaving e
  -- _
  --let e' = sendViaUsin_

runOmegaScoped ::
  ∀ param r .
  ( ∀ eff q z x
    . (forall y. z y -> Sem (eff ': Opaque q ': r) y)
   -> param eff z x -> Sem (Opaque q ': r) x) ->
  InterpreterFor (OmegaScoped param) r
runOmegaScoped interp = interpretH \case
  OmegaRunInScope w _ ->
    errorWithoutStackTrace $ "top level run with depth " ++ show w
  OmegaScoped (param :: param eff q a) n -> do
    (_ :: TypeParamsH z t sc r r) <- getTypeParamsH
    let
      runIt :: forall y
             . q y
            -> Sem (eff ': Opaque (Bundle '[OuterOmegaRun,
                                            HigherOrder z t sc r r]) ': r) y
      runIt = n 0
        >>> runH' @z @t @sc @r @r
        -- Scoped ... ': HigherOrder ... ': Bundle '[HigherOrder ...] ': r
        >>> sink @'[_]
        -- HigherOrder ... ': Scoped ... ': Bundle '[HigherOrder ...] ': r
        >>> rewrite (Bundle (Here @_ @_ @'[]))
        -- Bundle '[HigherOrder ...] ': Scoped ... ': Bundle '[HigherOrder ...] ': r
        >>> subsumeUsing (There Here)
        -- Scoped ... ': Bundle '[HigherOrder ...] ': r
        >>> go @eff 0
        -- effect k ': OuterRun effect ': Bundle '[HigherOrder ...] ': r
        >>> subsume_
        -- OuterRun effect ': Bundle '[HigherOrder ...] ': effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
        >>> consBundle @_ @'[_]
        -- Bundle '[OuterRun effect, HigherOrder ...] ': effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
        >>> subsumeUsing (There Here)
        -- effect k ': Bundle '[OuterRun effect, HigherOrder ...] ': r
        >>> bundleToOpaque
    interp runIt param
      & fromOpaque
      & runBundle
      & interpretH @OuterOmegaRun \case
        OuterOmegaRun w _ -> errorWithoutStackTrace $ "unhandled OuterOmegaRun with depth " ++ show w
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

    go :: forall eff l x
        . Word
       -> Sem (OmegaScoped param ': Bundle l ': r) x
       -> Sem (eff ': OuterOmegaRun ': Bundle l ': r) x
    go depth = reinterpret2H \case
      OmegaRunInScope w act
        | w == depth -> propagateUsing Here (unsafeCoerce act)
        | otherwise -> propagateUsing (There Here) (OuterOmegaRun w act)
      OmegaScoped (param :: param eff' q a) n -> do
        (_ :: TypeParamsH z t sc rPre rPost) <- getTypeParamsH
        let
          !depth' = depth + 1

          runIt :: forall y
                 . q y
                -> Sem (eff' ': Opaque (Bundle (OuterOmegaRun
                                               ': HigherOrder z t sc rPre rPost
                                                ': l)) ': r) y
          runIt = n depth'
            >>> runH' @z @t @sc @rPre @rPost
            -- Scoped ... ': HigherOrder ... ': Bundle l ': r
            >>> raiseUnder3
            -- Scoped ... ': HigherOrder ... ': Bundle l ': Bundle (HigherOrder ... ': l) ': r
            >>> sink @'[_, _]
            -- HigherOrder ... ': Bundle l ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
            >>> consBundle @_ @l
            -- Bundle (HigherOrder ... ': l) ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
            >>> subsumeUsing (There Here)
            -- Scoped ... ': Bundle '[HigherOrder ...] ': r
            >>> go @eff' depth'
            -- effect k' ': OuterRun effect ': Bundle (HigherOrder ... ': l') ': r
            >>> subsume_
            -- OuterRun effect ': Bundle (HigherOrder ... ': l') ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            >>> consBundle @OuterOmegaRun @(_ ': l)
            -- Bundle (OuterRun effect ': HigherOrder ... ': l) ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            >>> subsumeUsing (There Here)
            -- effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
            >>> bundleToOpaque

        interp runIt param
          & fromOpaque
          -- Bundle (OuterOmegaRun ': HigherOrder ... ': l) ': r
          & unconsBundle
          -- OuterOmegaRun ': Bundle (HigherOrder ... ': l) ': r
          & insertAt @2
          -- OuterOmegaRun ': Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect ': OuterOmegaRun ': Bundle l ': r
          & interpretH \case
            orun@(OuterOmegaRun w act)
              | w == depth -> propagateUsing (There (There Here)) (unsafeCoerce act)
              | otherwise -> propagateUsing (There (There (There Here))) orun
          -- Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect k ': OuterOmegaRun ': Bundle l ': r
          & unconsBundle
          -- HigherOrder ... ': Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
          & subsumeUsing (There Here)
          -- Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
          & subsumeUsing (There (There (There Here)))
          -- HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r


    -- go :: forall k l x
    --     . Word
    --    -> Sem (Scoped1 effect param modifier ': Bundle l ': r) x
    --    -> Sem (effect k ': OuterRun effect ': Bundle l ': r) x
    -- go depth = reinterpret2H \case
    --     RunInScope1 w act
    --       | w == depth -> propagateUsing Here (unsafeCoerce act)
    --       | otherwise -> propagate (OuterRun w act)
    --     Scoped1 ex (param :: param k') main -> do
    --       let depth' = depth + 1
    --       (_ :: TypeParamsH z t sc rPre rPost) <- getTypeParamsH
    --       runH' @z @t @sc @rPre @rPost (main depth')
    --         -- Scoped ... ': HigherOrder ... ': Bundle l ': r
    --         & raiseUnder3
    --         -- Scoped ... ': HigherOrder ... ': Bundle l ': Bundle (HigherOrder ... ': l) ': r
    --         & sink @'[_, _]
    --         -- HigherOrder ... ': Bundle l ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
    --         & consBundle @_ @l
    --         -- Bundle (HigherOrder ... ': l) ': Scoped ... ': Bundle (HigherOrder ... ': l) ': r
    --         & subsumeUsing (There Here)
    --         -- Scoped ... ': Bundle (HigherOrder ... ': l) ': r
    --         & go @k' depth'
    --         -- effect k' ': OuterRun effect ': Bundle (HigherOrder ... ': l') ': r
    --         & subsume_
    --         -- OuterRun effect ': Bundle (HigherOrder ... ': l') ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
    --         & consBundle @(OuterRun effect) @(_ ': l)
    --         -- Bundle (OuterRun effect ': HigherOrder ... ': l) ': effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
    --         & subsumeUsing (There Here)
    --         -- effect k' ': Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
    --         & bundleToOpaque
    --         -- effect k' ': Opaque (Bundle (OuterRun effect ': HigherOrder ... ': l)) ': r
    --         & interp param
    --         -- Opaque (Bundle (OuterRun effect ': HigherOrder ... ': l)) ': r
    --         & fromOpaque
    --         -- Bundle (OuterRun effect ': HigherOrder ... ': l) ': r
    --         & unconsBundle
    --         -- OuterRun effect ': Bundle (HigherOrder ... ': l) ': r
    --         & insertAt @2
    --         -- OuterRun effect ': Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
    --         & interpretH \case
    --           orun@(OuterRun w act)
    --             | w == depth -> propagateUsing (There (There Here)) (unsafeCoerce act)
    --             | otherwise -> propagate orun
    --         -- Bundle (HigherOrder ... ': l) ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
    --         & unconsBundle
    --         -- HigherOrder ... ': Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
    --         & subsumeUsing (There Here)
    --         -- Bundle l ': HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
    --         & subsumeUsing (There (There (There Here)))
    --         -- HigherOrder ... ': effect k ': OuterRun effect ': Bundle l ': r
    --       <&> ex
   -- => (forall eff z t q x y. Traversable t => param eff z x y -> Sem (eff ': HigherOrder z t q r r ': r) x -> Sem (HigherOrder z t q r r ': r) y)

-- type MetaParam = EffectRow -> Effect -> (Type -> Type) -> Effect

-- supergeneric but not worth it:
-- runMeta ::
--   ∀ param r .
--   ( ∀ eff q' z u x
--    -> param eff u z x -> Sem (MetaHandler r (RunHM z t r q' ': r)
--                               ': RunHM z t r q' ': r) x) ->
--   Sem (Meta q param ': r) x -> Sem (q ': r) x

{-

forall x. e z x -> Sem (HigherOrder z t e r r ': r) x

forall x. e z x -> Sem (HigherOrder z t e r r ': r) x
-}
-- this, instead:
-- runMeta ::
--   ∀ param r .
--   ( ∀ forall eff u x. InterpreterFor (param eff u) (RunHM eff u ': r))
-- runMeta ::
--   (forall eff u x. Sem (param eff u ': RunHM r) x -> Sem r x)
--   Sem (Meta param ': r) x -> Sem r x

data MetaHandler q z :: Effect where
  MetaHandlerMetaRun
    :: forall q z m a
     . MetaRun m a -> MetaHandler q z m a
  RunMeta
    :: forall q z m a
     . Unique
    -> q a
    -> MetaHandler q z m (z a)

data Box where
  Box :: a -> Box

newUnique' :: Box -> IO Unique
newUnique' (Box _) = newUnique
{-# NOINLINE newUnique' #-}

processMeta :: forall q z r a
             . q a
            -> Sem (MetaHandler q z ': r) (Unique, z a)
processMeta q = do
  let !uniq = unsafePerformIO (newUnique' (Box q))
      {-# NOINLINE uniq #-}
  z <- sendUsing Here (RunMeta uniq q)
  return (uniq, z)
{-# NOINLINE processMeta #-}

exposeMetaRun :: forall eff q z r a
               . ElemOf (MetaHandler q z) r
              -> Unique -> Sem r a -> Sem (eff ': r) a
exposeMetaRun pr uniq =
  raise
  >>> interceptUsingH (There pr) \case
        MetaHandlerMetaRun (MetaRun uniq' act)
          | uniq' == uniq -> propagateUsing Here (unsafeCoerce act)
        metarun -> propagateUsing (There pr) metarun

runMeta :: forall metaeff eff q t z rPre rPost r a mh
         . (Raise (mh ': rPost) r, mh ~ MetaHandler q z)
        => q a
        -> Sem (eff
                ': HigherOrder z t (Meta metaeff) (mh ': rPre) (mh ': rPost)
                ': r) a
runMeta q = do
  let mhMembership = There $ raiseMembership @(mh ': rPost) @r Here
  (uniq, z) <- subsumeUsing (There mhMembership) (processMeta q)
  exposeMetaRun mhMembership uniq (runH z)

runMeta' :: forall metaeff eff q t z rPre rPost r a mh
          . (Raise (mh ': rPre) r, mh ~ MetaHandler q z)
         => q a
         -> Sem (eff
                 ': Meta metaeff
                 ': HigherOrder z t (Meta metaeff) (mh ': rPre) (mh ': rPost)
                 ': r) a
runMeta' q = do
  let mhMembership = There $ There $ raiseMembership @(mh ': rPre) @r Here
  (uniq, z) <- subsumeUsing (There mhMembership) (processMeta q)
  exposeMetaRun mhMembership uniq (runH' z)

interpretMeta
  :: forall metaeff r
   . (   forall eff q t z x mh
       . (Traversable t, mh ~ MetaHandler q z)
      => metaeff eff q z x
      -> Sem (HigherOrder z t (Meta metaeff) (mh ': r) (mh ': r)
              ': mh ': r) x
     )
  -> InterpreterFor (Meta metaeff) r
interpretMeta = genericInterpretMeta idMembership

genericInterpretMeta
  :: forall metaeff rPre rPost
   . (forall e. ElemOf e rPre -> ElemOf e rPost)
  -> (   forall eff q t z y mh
       . (Traversable t, mh ~ MetaHandler q z)
      => metaeff eff q z y
      -> Sem (HigherOrder z t (Meta metaeff) (mh ': rPre) (mh ': rPost)
              ': mh ': rPost) y
     )
  -> (forall x. Sem (Meta metaeff ': rPre) x -> Sem rPost x)
genericInterpretMeta tPr h =
    interpretH \case
      MetaRun q _ ->
        errorWithoutStackTrace $
          "Unhandled MetaRun with unique hash " ++ show (hashUnique q)
  . genericInterpretH (There . tPr) \case
      MetaMetaRun metarun -> propagate metarun
      SendMeta metaeff (n :: forall y. Unique -> q y -> z y) ->
        h metaeff
        & (raiseUnder2 . raiseUnder2)
        & rewriteHigherOrder
        & subsumeUsing (There Here)
        & metaHandlerToOther (Bundle membership)
        where
          metaHandlerToOther
            :: forall r y
             . (forall k v. MetaRun k v -> Bundle r k v)
            -> Sem (MetaHandler q z ': r) y
            -> Sem r y
          metaHandlerToOther toBdl = interpretH \case
            MetaHandlerMetaRun metarun | Bundle pr act <- toBdl metarun ->
              propagateUsing pr act
            RunMeta uniq q -> return (n uniq q)

          rewriteHigherOrder
            :: forall t r y mh
             . mh ~ MetaHandler q z
            => Sem (HigherOrder z t (Meta metaeff) (mh ': rPre) (mh ': rPost)
                    ': r) y
            -> Sem (HigherOrder z t (Meta metaeff) rPre (MetaRun ': rPost)
                    ': r) y
          rewriteHigherOrder = reinterpret \case
            WithProcessorH main -> withProcessorH $ \lwr -> return $
              main $
                lwr
                >>> reinterpret2H \case
                  MetaMetaRun metarun ->
                    propagateUsing (There Here) (MetaHandlerMetaRun metarun)
                  sendmeta -> propagate sendmeta
            GetInterpreterH -> do
              InterpreterH interp <- getInterpreterH
              return $ InterpreterH $
                sink @'[_]
                >>> metaHandlerToOther (Bundle Here . MetaMetaRun)
                >>> interp
                >>> rewrite MetaHandlerMetaRun
            LiftWithH main -> liftWithH $ \lwr -> return $
              main (lwr . rewriteHigherOrder)
            RestoreH t -> restoreH t

  where
    sink :: forall mid l r' y before after
          . (before ~ l ': (Append mid r'), after ~ Append mid (l ': r'),
             Subsume before after
            )
         => Sem before y -> Sem after y
    sink = subsume_

-- transformUsing Here MetaHandlerMetaRun



-- runMeta ::
--   ∀ param r .
--   ( ∀ extra eff q z u x
--     . KnownList extra
--    => (forall y. z y -> Sem (Opaque q ': r) y)
--    -> (forall y. u y -> Sem (eff ': Opaque q ': r) y)
--    -> param extra eff z u x -> Sem (Append extra (Opaque q ': r)) x) ->
--   InterpreterFor (Meta param) r
-- runMeta interp =
--   raiseUnder
--   >>> go 0
--   >>> interpretH \case
--     OuterMetaRun w _ ->
--       errorWithoutStackTrace $ "Unhandled MetaRun with depth " ++ show w
--     OuterMetaBundle w _ ->
--       errorWithoutStackTrace $ "Unhandled MetaBundle with depth " ++ show w
--   >>> runBundle @'[]
--   where
--     sink :: forall mid l r' x before after
--           . (before ~ l ': (Append mid r'), after ~ Append mid (l ': r'),
--              Subsume before after
--             )
--          => Sem before x -> Sem after x
--     sink = subsume_

--     bundleToOpaque :: forall e l x
--                     . Sem (e ': Bundle l ': r) x
--                    -> Sem (e ': Opaque (Bundle l) ': r) x
--     bundleToOpaque = coerceEffs

--     go :: forall l x
--         . Word
--        -> Sem (Meta param ': Bundle l ': r) x
--        -> Sem (OuterMeta ': Bundle l ': r) x
--     go depth = reinterpretH \case
--       MetaRun w act -> propagate (OuterMetaRun w act)
--       MetaBundle w bdl -> propagate (OuterMetaBundle w bdl)
--       ToMeta slist (param :: param extra eff' q p a) n1 n2 razer -> do
--         (_ :: TypeParamsH z t meta rPre rPost) <- getTypeParamsH
--         (_ :: Proxy opaque) <-
--           return $ Proxy @(Opaque (Bundle (HigherOrder z t meta rPre rPost
--                                            ': OuterMeta
--                                            ': l)))
--         let
--           !depth' = depth + 1

--           runIt1 :: forall y. z y -> Sem (eff' ': opaque ': r) y
--           runIt1 = runH' @z @t @meta @rPre @rPost
--             -- Meta ... ': HigherOrder ... ': Bundle l ': r
--             >>> raiseUnder3
--             -- Meta ... ': HigherOrder ... ': Bundle l ': Bundle (HigherOrder ... ': l) ': r
--             >>> sink @'[_, _]
--             -- HigherOrder ... ': Bundle l ': Meta ... ': Bundle (HigherOrder ... ': l) ': r
--             >>> transformUsing (There (There Here)) (Bundle Here)
--             -- Bundle l ': Meta ... ': Bundle (HigherOrder ... ': l) ': r
--             >>> transformUsing
--                   (There Here)
--                   (\(Bundle pr act) -> Bundle (There (There pr)) act)
--             -- Meta ... ': Bundle (HigherOrder ... ': l) ': r
--             >>> go depth'
--             -- OuterMetaRun ': Bundle (HigherOrder ... ': l) ': r
--             >>> reinterpretH \case
--               OuterMetaRun w act
--                 | depth == w -> propagateUsing Here (unsafeCoerce act)
--               outermeta -> propagateUsing (There Here) (Bundle (There Here) outermeta)
--             -- effect ': Bundle (HigherOrder ... ': l) ': r
--             >>> bundleToOpaque

--           runIt2 :: forall y. z y -> Sem (opaque ': r) y
--           runIt2 = runH @z @t @meta @rPre @rPost
--             -- HigherOrder ... ': OuterMeta ': Bundle l ': r
--             >>> raiseUnder3
--             -- HigherOrder ... ': OuterMeta ': Bundle l ': (Bundle ...) ': 'r
--             >>> transformUsing (There (There Here)) (Bundle Here) -- interpretH \case
--             -- OuterMeta ': Bundle l ': (Bundle ...) ': 'r
--             >>> transformUsing (There Here) (Bundle (There Here))
--             -- Bundle l ': (Bundle ...) ': 'r
--             >>> transformUsing
--                   Here
--                   (\(Bundle pr act) -> Bundle (There (There pr)) act)
--             -- (Bundle ...) ': 'r
--             >>> toOpaque

--           handleOuterMetaBundle
--             :: forall z' t' r' y
--              . r' ~ (HigherOrder z t meta rPre rPost ': OuterMeta ': Bundle l ': r)
--             => OuterMeta z' y
--             -> Sem (HigherOrder z' t' OuterMeta r' r' ': r') y
--           handleOuterMetaBundle = \case
--             OuterMetaBundle w (Bundle (pr :: ElemOf e' rFinal) act)
--               | w == depth,
--                 UnsafeRefl <- unsafeEqualityProof @rFinal @(opaque ': r) ->
--                 case pr of
--                   Here -> case act of
--                     -- Opaque (Bundle Here (RestoreH t)) -> case traverse (const Nothing) t of
--                     --   Just tVoid -> propagateUsing Here (RestoreH tVoid)
--                     --   _          -> return (foldr const undefined t)
--                     Opaque (Bundle Here act') -> propagate act'
--                     Opaque (Bundle (There Here) act') ->
--                       handleOuterMetaBundle act'
--                     Opaque (Bundle (There (There pr')) act') ->
--                       propagate (Bundle pr' act')
--                   There pr' -> propagateUsing (There (There (There pr'))) act
--             outermeta -> propagate outermeta

--         case toKnownList slist of
--           Dict ->
--             interp (runIt2 . n1) (runIt1 . n2 depth) param
--               & getRazer razer depth runIt2
--               -- Opaque (Bundle (HigherOrder z t mt rPre rPost ': OuterMeta ': l)) ': r
--               & fromOpaque
--               -- Bundle (HigherOrder z t mt rPre rPost ': OuterMeta ': l) ': r
--               & reinterpret3H \case
--                   Bundle Here e -> propagate e
--                   Bundle (There Here) e -> propagate e
--                   Bundle (There (There pr)) e -> propagate (Bundle pr e)
--               -- HigherOrder z t mt rPre rPost ': OuterMeta ': Bundle l ': r
--               & interceptH handleOuterMetaBundle
--               -- HigherOrder z t mt rPre rPost ': OuterMeta ': Bundle l ': r
