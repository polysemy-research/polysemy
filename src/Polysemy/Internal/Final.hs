{-# LANGUAGE TemplateHaskell, PatternGuards, EmptyCase #-}
module Polysemy.Internal.Final
  (
    -- * Effect
    Final(..)

    -- * Actions
  , withTransToFinal
  , controlFinal
  , withLoweringToFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Lowering
    -- | 'Lowering' is a domain-specific language very similar in
    -- use to 'Polysemy.HigherOrder', and is used to describe how
    -- higher-order effects are threaded down to the final monad.
    --
    -- 'withLoweringToFinal' should be used when 'controlFinal' is
    -- not powerful enough for your purposes. Notable combinators include
    -- 'runL', 'embed', 'liftWithL', and 'restoreL'.
  , Lowering

    -- ** Lifting the final monad to 'Lowering'
  , embed

    -- ** Lowering computations to the final monad
  , withProcessorL
  , controlWithProcessorL
  , processL

    -- ** Embedding computations into 'Lowering'
  , runL

    -- ** Lowering 'Lowering' to the final monad
  , controlL
  , liftWithL

    -- ** Manipulating effectful state
  , restoreL
  , runExposeL
  , exposeL

    -- * Interpretations
  , runM
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal

  -- * Retrieving the type parameters of a 'Lowering' environemt
  , TypeParamsL(..)
  , getTypeParamsL
  ) where

import Data.Functor.Identity
import Data.Kind
import Control.Monad.Trans
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.TH.Effect
import Polysemy.Internal.HigherOrder (interpret)

-----------------------------------------------------------------------------
-- | An effect for embedding higher-order actions in the final target monad
-- of the effect stack.
--
-- This is very useful for writing interpreters that interpret higher-order
-- effects in terms of the final monad.
--
-- 'Final' is more powerful than 'Embed', but is also less flexible
-- to interpret (compare 'Polysemy.Embed.embedToEmbed' with 'finalToFinal').
-- If you only need the power of 'embed', then you should use 'Embed' instead.
--
-- /Note/: 'Final' actions are interpreted as actions of the final monad,
-- and the effectful state visible to 'controlFinal' \/ 'withLoweringToFinal'
-- \/ 'interpretFinal' is that of /all interpreters run in order to produce the/
-- /final monad/.
--
-- This means that any interpreter built using 'Final' will /not/ respect
-- local/global state semantics based on the order of interpreters run.
-- In this library, interpreters that make use of 'Final' signal this by having
-- a @-'Final'@ suffix.
--
-- State semantics of effects that are /not/ interpreted in terms of the final
-- monad will always appear local to effects that are interpreted in terms of
-- the final monad.
--
-- State semantics between effects that are interpreted in terms of the final
-- monad depend on the final monad. For example, if the final monad is a monad
-- transformer stack, then state semantics will depend on the order monad
-- transformers are stacked.
--
-- @since 1.2.0.0
newtype Final m z a where
  WithTransToFinal
    :: (forall t. MonadTransWeave t => (forall x. z x -> t m x) -> t m a)
    -> Final m z a

makeSem ''Final

-- | The 'Lowering' environment, which is a 'Polysemy.Sem' with a small
-- effect stack containing the @'Lower' m t n@ effect, which provides the
-- various @-S@ combinators, as well as @'Embed' m@ and @'Final' m@ effects,
-- allowing you to embed actions of the final monad into 'Lowering'.
--
-- The type parameters of 'Lowering' represent the following:
--
-- * @m@: The final monad, the @m@ in @'Final' m@.
-- * @t@: The type of final effectful state once /all/ interpreters -- past
--   and future -- have been run, and @'Polysemy.Sem'@ has been reduced to the
--   final monad.
-- * @n@: The type of the \"source monad\": the monad that can be lowered t
--   @m@. When 'withLoweringToFinal' is used, @n@ is @Sem r@.
--
-- @since 2.0.0.0
type Lowering m t n = Sem '[Lower m t n, Embed m, Final m]

data Lower m t n z a where
  LiftWithL :: forall m t n z a
             . ((forall x. Lowering m t n x -> m (t x)) -> m a)
            -> Lower m t n z a
  WithProcessorL :: forall m t n z a
                  . ((forall x. n x -> m (t x)) -> m a)
                 -> Lower m t n z a
  RestoreL  :: forall m t n z a. t a -> Lower m t n z a
  RunL      :: forall m t n z a. n a -> Lower m t n z a

-- | A singleton datatype parametrized with type parameters corresponding to
-- @HigherOrder@
data TypeParamsL
      (m :: Type -> Type)
      (t :: Type -> Type)
      (n :: Type -> Type) = TypeParamsL

-- | A trivial action just returning the 'TypeParamsL' singleton, with
-- type parameters matching that of the current 'Lowering' environment.
--
-- You can use this together with @ScopedTypeVariables@ to gain access to the
-- various parameters of the 'Lowering' if you need them.
getTypeParamsL :: forall m t n r
                . Sem (Lower m t n ': r) (TypeParamsL m t n)
getTypeParamsL = return TypeParamsL

-- | Embed a computation of the source monad into 'Lowering'.
--
-- @since 2.0.0.0
runL :: forall m t n r a. n a -> Sem (Lower m t n ': r) a
runL = send . RunL @m @t
-- {-# INLINE runL #-}

-- | Lift an computation of the final monad @m@ by giving it access to a
-- lowering function that can transform @'Lowering' m t n x@ to @m (t x)@.
--
-- This is analogous to @liftBaseWith@ of @MonadBaseControl@.
--
-- Note: the lowering function lowers @'Lowering' m t n@ by using the
-- effectful state as it is when 'liftWithL' is run.
--
-- @since 2.0.0.0
liftWithL :: forall m t n r a
           . ((forall x. Lowering m t n x -> m (t x)) -> m a)
          -> Sem (Lower m t n ': r) a
liftWithL main = send (LiftWithL main)
-- {-# INLINE liftWithL #-}

-- | A particularly useful composition:
-- @'controlL' h = 'liftWithL' h >>= 'restoreL'@
--
-- This is analogous to @control@ of @MonadBaseControl@.
--
-- Note: the lowering function lowers @'Lowering' m t n@ by using the
-- effectful state as it is when 'controlL' is run.
--
-- @since 2.0.0.0
controlL :: forall m t n r a
          . ((forall x. Lowering m t n x -> m (t x)) -> m (t a))
         -> Sem (Lower m t n ': r) a
controlL main = liftWithL main >>= restoreL
-- {-# INLINE controlL #-}

-- | Embed a computation of the source monad into 'Lowering', and reify
--   the effectful state of all purely interpreted effects (effects not
--   ultimately interpreted in terms of the final monad).
--
-- By reifying the effectful state, you may do one or more of the following:
--
-- * Guarantee that execution won't be interrupted by the failure of a purely
--   interpreted effect, since that failure will instead be reified into the
--   state.
-- * Check if the computation has failed because of a purely interpreted effect
--   by using 'Data.Foldable.null'
--   or @`Data.Traversable.traverse` (const Nothing)@.
-- * Discard any impact the computation has on purely interpreted effects by
--   never restoring the effectful state.
--
-- Once an effectful state has been reified, you may restore it using
-- 'restoreL'.
--
-- @since 2.0.0.0
runExposeL :: forall m t n r a. n a -> Sem (Lower m t n ': r) (t a)
runExposeL z = withProcessorL $ \lower -> lower z
-- {-# INLINE runExposeL #-}

-- | Restore a reified effectful state, bringing its changes into scope, and
-- returning the result of the computation.
--
-- This is analogous to @restoreM@ of @MonadBaseControl@.
--
-- /Note/: this overrides the local effectful state of any previously restored
-- effectful state.
--
-- For example, consider:
--
-- @
-- ta <- 'runExposeL' ma
-- tb <- 'runExposeL' mb
-- _  <- 'restoreL' ta
-- _  <- 'restoreL' tb
-- @
--
-- Unless @'restoreL' ta@ causes the handler to fail (because @ma@ failed due to
-- a local effect), the changes it brings into scope will be overridden by
-- @'restoreL' tb@.
--
-- If you want to integrate the results of both actions, you need to restore the
-- state in between uses of 'runExposeL', so that @'runExposeL' mb@ works with
-- the changes of @ta@ in scope.
-- @
-- ta <- 'runExposeH' ma
-- _  <- 'restoreL' ta
-- tb <- 'runExposeH' mb
-- _  <- 'restoreH' tb
-- @
--
-- @since 2.0.0.0
restoreL :: forall m t n r a. t a -> Sem (Lower m t n ': r) a
restoreL = send . RestoreL @m @_ @n
-- {-# INLINE restoreL #-}

-- | Reify the effectful state of the purely interpreted effects used
--   used within the argument.
--
-- @'runExposeL' m = 'exposeL' ('runL' m)@
--
-- @since 2.0.0.0
exposeL :: forall m t n r a
         . Lowering m t n a
        -> Sem (Lower m t n ': r) (t a)
exposeL m = liftWithL $ \lower -> lower m
-- {-# INLINE exposeL #-}

-- | Process a computation of the source monad by turning it into an computation
-- of the final monad returning a reified effectful state. The processed
-- computation is returned, rather than being immediately run like with 'runL'.
--
-- /Note/: The processed action makes use of the effectful state as it is by
-- the time 'processL' is run, rather than what it is by the time the processed
-- computation is run.
--
-- @since 2.0.0.0
processL :: forall m t n r a
          . Monad m
         => n a
         -> Sem (Lower m t n ': r) (m (t a))
processL z = withProcessorL $ \lower -> return (lower z)
-- {-# INLINE processL #-}

-- | Lift an computation of the final monad by giving it access to a processor:
-- a function that transforms a computation of the source monad by turning it
-- into a computation of the final monad returning a reified effectful state.
--
-- 'withProcessorL' is a useful specialization of 'liftWithL':
--
-- @'withProcessorL' main = 'liftWithL' (\n -> main (n . 'runL'))@
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'withProcessorL' is run, rather than what it is by the time the
-- processed action is run.
--
-- @since 2.0.0.0
withProcessorL :: forall m t n r a
                . ((forall x. n x -> m (t x)) -> m a)
               -> Sem (Lower m t n ': r) a
withProcessorL main = send (WithProcessorL main)
-- {-# INLINE withProcessorL #-}

-- | A particularly useful composition:
-- @'controlWithProcessorL' h = 'withProcessorL' h >>= 'restoreL'@
--
-- 'controlWithProcessorL' is a useful specialization of 'controlL':
--
-- @'controlWithProcessorL' main = 'controlL' (\n -> main (n . 'runL'))@
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'controlWithProcessorL' is run, rather than what it is by the time
-- the processed action is run.
--
-- @since 2.0.0.0
controlWithProcessorL :: forall m t n r a
                       . ((forall x. n x -> m (t x)) -> m (t a))
                      -> Sem (Lower m t n ': r) a
controlWithProcessorL main = withProcessorL main >>= restoreL
-- {-# INLINE controlWithProcessorL #-}

-- | Lift an computation of the final monad by giving it access to a function
-- that transforms any @'Sem' r@ computation into a compuation of the final
-- monad returning a reified effectful state. The computation of the final
-- monad must return a resulting effectful state.
--
-- The reified effectful state @t@ is 'Traversable', which gives you some
-- options -- see 'runExposeL'.
--
-- 'controlFinal' provides no means of sequentially combining effectful states.
-- If you need transform multiple @'Polysemy.Sem' r@ computations to the final
-- monad and sequentially execute them, then 'withLoweringToFinal' should be
-- used instead.
--
-- You are discouraged from using 'controlFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @'controlFinal' main = 'withLoweringToFinal' ('controlWithProcessorL' main)@
--
-- @since 2.0.0.0
controlFinal :: forall m r a
              . (Member (Final m) r, Monad m)
             => (  forall t
                 . Traversable t
                => (forall x. Sem r x -> m (t x)) -> m (t a)
                )
             -> Sem r a
controlFinal main = withTransToFinal $ \n ->
  controlT $ \lower -> main (lower . n)
-- {-# INLINE controlFinal #-}

runLowering :: forall m n t a
             . (Monad m, MonadTransWeave t)
            => Lowering m (StT t) n a
            -> (forall x. n x -> t m x) -> t m a
runLowering main nat =
  let
    go :: forall x. Lowering m (StT t) n x -> t m x
    go = usingSem $ \(Union pr (Weaving eff mkT lwr ex)) -> do
      let run_it = (ex . (<$ mkInitState lwr))
      case pr of
        Here -> run_it <$> case eff of
          RestoreL t -> restoreT (return t)
          RunL m -> nat m
          LiftWithL main' -> liftWith $ \lower -> main' (lower . go)
          WithProcessorL main' -> liftWith $ \lower -> main' (lower . nat)
        There Here | Embed m <- eff -> run_it <$> lift m
        There (There Here) | WithTransToFinal main' <- eff ->
          fmap ex $ lwr $ getComposeT $ main' (ComposeT . mkT go)
        There (There (There pr_)) -> case pr_ of {}
  in
    go main

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad. This is done through the use of the 'Lower'
-- environment, which provides a variety of combinators, most notably
-- 'controlL'.
--
-- You are discouraged from using 'withLoweringToFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @since 2.0.0.0
withLoweringToFinal :: (Monad m, Member (Final m) r)
                     => (forall t. Traversable t => Lowering m t (Sem r) a)
                     -> Sem r a
withLoweringToFinal main = withTransToFinal (runLowering main)
-- {-# INLINE withLoweringToFinal #-}

------------------------------------------------------------------------------
-- | Lower a 'Sem' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Sem '[Embed m, Final m] a -> m a
runM = usingSem $ \u -> case decomp u of
  Right (Weaving (Embed m) _ lwr ex) -> fmap (ex . (<$ mkInitState lwr)) m
  Left g -> case extract g of
    Weaving (WithTransToFinal main) mkT lwr ex ->
      fmap ex $ lwr $ main $ mkT runM
-- {-# INLINE runM #-}
{-# SPECIALIZE runM :: Sem '[Embed IO, Final IO] a -> IO a #-}
{-# SPECIALIZE runM :: Sem '[Embed Identity, Final Identity] a -> Identity a #-}


-----------------------------------------------------------------------------
-- | 'withLoweringToFinal' admits an implementation of 'embed'.
--
-- Just like 'embed', you are discouraged from using this in application code.
--
-- @since 1.2.0.0
embedFinal :: (Member (Final m) r, Monad m) => m a -> Sem r a
embedFinal m = withTransToFinal $ \_ -> lift m
-- {-# INLINE embedFinal #-}

------------------------------------------------------------------------------
-- | Like 'interpretH', but may be used to
-- interpret higher-order effects in terms of the final monad.
--
-- 'interpretFinal' requires less boilerplate than using 'interpretH'
-- together with 'withLoweringToFinal', but is also less powerful.
-- 'interpretFinal' does not provide any means of executing actions
-- of @'Sem' r@ as you interpret each action, and the provided interpreter
-- is automatically recursively used to process higher-order occurences of
-- @'Sem' (e ': r)@ to @'Sem' r@.
--
-- If you need greater control of how the effect is interpreted,
-- use 'interpretH' together with 'withLoweringToFinal' \/
-- 'controlFinal' instead.
--
-- /Note/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to effects
-- interpreted using 'interpretFinal'. See 'Final'.
--
-- @since 2.0.0.0
interpretFinal
    :: forall m e r a
     . (Member (Final m) r, Monad m)
    => (forall t z x. Traversable t => e z x -> Lowering m t z x)
       -- ^ A handler written using the 'Lowering' environment, where
       -- the source monad @z@ is the monad of the higher-order chunks in @e@.
    -> Sem (e ': r) a
    -> Sem r a
interpretFinal h =
  let
    go :: Sem (e ': r) x -> Sem r x
    go = hoistSem $ \u -> case decomp u of
      Right (Weaving e mkT lwr ex) ->
        injWeaving $
          Weaving
            (WithTransToFinal (runLowering (h e)))
            (\n -> mkT (n . go))
            lwr
            ex
      Left g -> hoist go g
    -- {-# INLINE go #-}
  in
    go
-- {-# INLINE interpretFinal #-}

------------------------------------------------------------------------------
-- | Given natural transformations between @m1@ and @m2@, run a @'Final' m1@
-- effect by transforming it into a @'Final' m2@ effect.
--
-- @since 1.2.0.0
finalToFinal :: forall m1 m2 r a
              . (Monad m1, Monad m2, Member (Final m2) r)
             => (forall x. m1 x -> m2 x)
             -> (forall x. m2 x -> m1 x)
             -> Sem (Final m1 ': r) a
             -> Sem r a
finalToFinal to from =
  let
    go :: Sem (Final m1 ': r) x -> Sem r x
    go = hoistSem $ \u -> case decomp u of
      Right (Weaving (WithTransToFinal main) mkT lwr ex) ->
        injWeaving $
          Weaving
            (WithTransToFinal $ \n -> hoistT to $ main (hoistT from . n))
            (\n -> mkT (n . go))
            lwr
            ex
      Left g -> hoist go g
    -- {-# INLINE go #-}
  in
    go
-- {-# INLINE finalToFinal #-}

------------------------------------------------------------------------------
-- | Transform an @'Embed' m@ effect into a @'Final' m@ effect
--
-- @since 1.2.0.0
embedToFinal :: (Member (Final m) r, Monad m)
             => Sem (Embed m ': r) a
             -> Sem r a
embedToFinal = interpret $ \(Embed m) -> embedFinal m
-- {-# INLINE embedToFinal #-}
