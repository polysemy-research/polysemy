{-# LANGUAGE TemplateHaskell, PatternGuards, EmptyCase #-}
module Polysemy.Internal.Final
  (
    -- * Effect
    Final(..)

    -- * Actions
  , withLoweringToFinal
  , controlFinal
  , withStrategicToFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Strategy
    -- | 'Strategic' is a domain-specific language very similar in
    -- use to 'Polysemy.Interpretation.Handling', and is used to describe how
    -- higher-order effects are threaded down to the final monad.
    --
    -- 'withStrategicToFinal' should be used when 'controlFinal' is
    -- not powerful enough for your purposes. Notable combinators include
    -- 'runS', 'embed', 'liftWithS', and 'restoreS'.
  , Strategic

    -- ** Lifting the final monad to 'Strategic'
  , embed

    -- ** Lowering computations to the final monad
  , withProcessorS
  , controlWithProcessorS
  , processS

    -- ** Embedding computations into 'Strategic'
  , runS

    -- ** Lowering 'Strategic' to the final monad
  , controlS
  , liftWithS

    -- ** Manipulating effectful state
  , restoreS
  , runExposeS
  , exposeS

    -- * Interpretations
  , runM
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal
  ) where

import Control.Monad.Trans
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.TH.Effect
import Polysemy.Internal.Interpret (interpret)

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
-- and the effectful state visible to 'controlFinal' \/ 'withStrategicToFinal'
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
  WithLoweringToFinal
    :: (forall t. MonadTransWeave t => (forall x. z x -> t m x) -> t m a)
    -> Final m z a

makeSem ''Final

-- | The 'Strategic' environment, which is a 'Polysemy.Sem' with a small
-- effect stack containing the @'Strategy' m t n@ effect, which provides the
-- various @-S@ combinators, as well as @'Embed' m@ and @'Final' m@ effects,
-- allowing you to embed actions of the final monad into 'Strategic'.
--
-- The type parameters of 'Strategic' represent the following:
--
-- * @m@: The final monad, the @m@ in @'Final' m@.
-- * @t@: The type of final effectful state once /all/ interpreters -- past
--   and future -- have been run, and @'Polysemy.Sem'@ has been reduced to the
--   final monad.
-- * @n@: The type of the \"source monad\": the monad that can be lowered t
--   @m@. When 'withStrategicToFinal' is used, @n@ is @Sem r@.
--
-- @since 2.0.0.0
type Strategic m t n = Sem '[Strategy m t n, Embed m, Final m]

data Strategy m t n z a where
  LiftWithS :: forall m t n z a
             . ((forall x. Strategic m t n x -> m (t x)) -> m a)
            -> Strategy m t n z a
  WithProcessorS :: forall m t n z a
                  . ((forall x. n x -> m (t x)) -> m a)
                 -> Strategy m t n z a
  RestoreS  :: forall m t n z a. t a -> Strategy m t n z a
  RunS      :: forall m t n z a. n a -> Strategy m t n z a

-- | Embed a computation of the source monad into 'Strategic'.
--
-- @since 2.0.0.0
runS :: forall m t n r a. n a -> Sem (Strategy m t n ': r) a
runS = send . RunS @m @t
{-# INLINE runS #-}

-- | Lift an computation of the final monad @m@ by giving it access to a
-- lowering function that can transform transform @'Strategic' m t n x@ to
-- @m (t x)@.
--
-- This is analogous to @liftBaseWith@ of @MonadBaseControl@.
--
-- Note: the lowering function lowers @'Strategic' m t n@ by using the
-- effectful state as it is when 'liftWithS' is run.
--
-- @since 2.0.0.0
liftWithS :: forall m t n r a
           . ((forall x. Strategic m t n x -> m (t x)) -> m a)
          -> Sem (Strategy m t n ': r) a
liftWithS main = send (LiftWithS main)
{-# INLINE liftWithS #-}

-- | A particularly useful composition:
-- @'controlS' h = 'liftWithS' h >>= 'restoreS'@
--
-- This is analogous to @control@ of @MonadBaseControl@.
--
-- Note: the lowering function lowers @'Strategic' m t n@ by using the
-- effectful state as it is when 'controlS' is run.
--
-- @since 2.0.0.0
controlS :: forall m t n r a
          . ((forall x. Strategic m t n x -> m (t x)) -> m (t a))
         -> Sem (Strategy m t n ': r) a
controlS main = liftWithS main >>= restoreS
{-# INLINE controlS #-}

-- | Embed a computation of the source monad into 'Strategic', and reify
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
-- 'restoreS'.
--
-- @since 2.0.0.0
runExposeS :: forall m t n r a. n a -> Sem (Strategy m t n ': r) (t a)
runExposeS z = withProcessorS $ \lower -> lower z
{-# INLINE runExposeS #-}

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
-- ta <- 'runExposeS' ma
-- tb <- 'runExposeS' mb
-- _  <- 'restoreS' ta
-- _  <- 'restoreS' tb
-- @
--
-- Unless @'restoreS' ta@ causes the handler to fail (because @ma@ failed due to
-- a local effect), the changes it brings into scope will be overridden by
-- @'restoreS' tb@.
--
-- If you want to integrate the results of both actions, you need to restore the
-- state in between uses of 'runExposeS', so that @'runExposeS' mb@ works with
-- the changes of @ta@ in scope.
-- @
-- ta <- 'runExposeH' ma
-- _  <- 'restoreS' ta
-- tb <- 'runExposeH' mb
-- _  <- 'restoreH' tb
-- @
--
-- @since 2.0.0.0
restoreS :: forall m t n r a. t a -> Sem (Strategy m t n ': r) a
restoreS = send . RestoreS @m @_ @n
{-# INLINE restoreS #-}

-- | Reify the effectful state of the purely interpreted effects used
--   used within the argument.
--
-- @'runExposeS' m = 'exposeS' ('runS' m)@
--
-- @since 2.0.0.0
exposeS :: forall m t n r a
         . Strategic m t n a
        -> Sem (Strategy m t n ': r) (t a)
exposeS m = liftWithS $ \lower -> lower m
{-# INLINE exposeS #-}

-- | Process a computation of the source monad by turning it into an computation
-- of the final monad returning a reified effectful state. The processed
-- computation is returned, rather than being immediately run like with 'runS'.
--
-- /Note/: The processed action makes use of the effectful state as it is by
-- the time 'processS' is run, rather than what it is by the time the processed
-- computation is run.
--
-- @since 2.0.0.0
processS :: forall m t n r a
          . Monad m
         => n a
         -> Sem (Strategy m t n ': r) (m (t a))
processS z = withProcessorS $ \lower -> return (lower z)
{-# INLINE processS #-}

-- | Lift an computation of the final monad by giving it access to a processor:
-- a function that transforms a computation of the source monad by turning it
-- into a computation of the final monad returning a reified effectful state.
--
-- 'withProcessorS' is a useful specialization of 'liftWithS':
--
-- @'withProcessorS' main = 'liftWithS' (\n -> main (n . 'runS'))@
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'withProcessorS' is run, rather than what it is by the time the
-- processed action is run.
--
-- @since 2.0.0.0
withProcessorS :: forall m t n r a
                . ((forall x. n x -> m (t x)) -> m a)
               -> Sem (Strategy m t n ': r) a
withProcessorS main = send (WithProcessorS main)
{-# INLINE withProcessorS #-}

-- | A particularly useful composition:
-- @'controlWithProcessorS' h = 'withProcessorS' h >>= 'restoreS'@
--
-- 'withProcessorS' is a useful specialization of 'controlS':
--
-- @'controlWithProcessorS' main = 'controlS' (\n -> main (n . 'runS'))@
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'controlWithProcessorS' is run, rather than what it is by the time
-- the processed action is run.
--
-- @since 2.0.0.0
controlWithProcessorS :: forall m t n r a
                       . ((forall x. n x -> m (t x)) -> m (t a))
                      -> Sem (Strategy m t n ': r) a
controlWithProcessorS main = withProcessorS main >>= restoreS
{-# INLINE controlWithProcessorS #-}

-- | Lift an computation of the final monad by giving it access to a function
-- that transforms any @'Sem' r@ computation into a compuation of the final
-- monad returning a reified effectful state. The computation of the final
-- monad must return a resulting effectful state.
--
-- The reified effectful state @t@ is 'Traversable', which gives you some
-- options -- see 'runExposeS'.
--
-- 'controlFinal' provides no means of sequentially combining effectful states.
-- If you need transform multiple @'Polysemy.Sem' r@ computations to the final
-- monad and sequentially execute them, then 'withStrategicToFinal' should be
-- used instead.
--
-- You are discouraged from using 'controlFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @'controlFinal' main = 'withStrategicToFinal' ('controlWithProcessorS' main)@
--
-- @since 2.0.0.0
controlFinal :: forall m r a
              . (Member (Final m) r, Monad m)
             => (  forall t
                 . Traversable t
                => (forall x. Sem r x -> m (t x)) -> m (t a)
                )
             -> Sem r a
controlFinal main = withLoweringToFinal $ \n ->
  controlT $ \lower -> main (lower . n)
{-# INLINE controlFinal #-}

runStrategy :: forall m n t a
             . (Monad m, MonadTransWeave t)
            => Strategic m (StT t) n a
            -> (forall x. n x -> t m x) -> t m a
runStrategy main nat =
  let
    go :: forall x. Strategic m (StT t) n x -> t m x
    go = usingSem $ \(Union pr (Weaving eff mkT lwr ex)) -> do
      let run_it = (ex . (<$ mkInitState lwr))
      case pr of
        Here -> run_it <$> case eff of
          RestoreS t -> restoreT (return t)
          RunS m -> nat m
          LiftWithS main' -> liftWith $ \lower -> main' (lower . go)
          WithProcessorS main' -> liftWith $ \lower -> main' (lower . nat)
        There Here | Embed m <- eff -> run_it <$> lift m
        There (There Here) | WithLoweringToFinal main' <- eff ->
          fmap ex $ lwr $ getComposeT $ main' (ComposeT . mkT go)
        There (There (There pr_)) -> case pr_ of {}
  in
    go main

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad. This is done through the use of the 'Strategy'
-- environment, which provides a variety of combinators, most notably
-- 'controlS'.
--
-- You are discouraged from using 'withStrategicToFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @since 2.0.0.0
withStrategicToFinal :: (Monad m, Member (Final m) r)
                     => (forall t. Traversable t => Strategic m t (Sem r) a)
                     -> Sem r a
withStrategicToFinal main = withLoweringToFinal (runStrategy main)
{-# INLINE withStrategicToFinal #-}

------------------------------------------------------------------------------
-- | Lower a 'Sem' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Sem '[Embed m, Final m] a -> m a
runM = usingSem $ \u -> case decomp u of
  Right (Weaving (Embed m) _ lwr ex) -> fmap (ex . (<$ mkInitState lwr)) m
  Left g -> case extract g of
    Weaving (WithLoweringToFinal main) mkT lwr ex ->
      fmap ex $ lwr $ main $ mkT runM
{-# INLINE runM #-}


-----------------------------------------------------------------------------
-- | 'withStrategicToFinal' admits an implementation of 'embed'.
--
-- Just like 'embed', you are discouraged from using this in application code.
--
-- @since 1.2.0.0
embedFinal :: (Member (Final m) r, Monad m) => m a -> Sem r a
embedFinal m = withLoweringToFinal $ \_ -> lift m
{-# INLINE embedFinal #-}

------------------------------------------------------------------------------
-- | Like 'interpretH', but may be used to
-- interpret higher-order effects in terms of the final monad.
--
-- 'interpretFinal' requires less boilerplate than using 'interpretH'
-- together with 'withStrategicToFinal' \/ 'withWeavingToFinal',
-- but is also less powerful.
-- 'interpretFinal' does not provide any means of executing actions
-- of @'Sem' r@ as you interpret each action, and the provided interpreter
-- is automatically recursively used to process higher-order occurences of
-- @'Sem' (e ': r)@ to @'Sem' r@.
--
-- If you need greater control of how the effect is interpreted,
-- use 'interpretH' together with 'withStrategicToFinal' \/
-- 'withWeavingToFinal' instead.
--
-- /Note/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to effects
-- interpreted using 'interpretFinal'. See 'Final'.
--
-- @since 2.0.0.0
interpretFinal
    :: forall m e r a
     . (Member (Final m) r, Monad m)
    => (forall t z x. Traversable t => e z x -> Strategic m t z x)
       -- ^ A handler written using the 'Strategic' environment, where
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
            (WithLoweringToFinal (runStrategy (h e)))
            (\n -> mkT (n . go))
            lwr
            ex
      Left g -> hoist go g
    {-# INLINE go #-}
  in
    go
{-# INLINE interpretFinal #-}

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
      Right (Weaving (WithLoweringToFinal main) mkT lwr ex) ->
        injWeaving $
          Weaving
            (WithLoweringToFinal $ \n -> hoistT to $ main (hoistT from . n)
            )
            (\n -> mkT (n . go))
            lwr
            ex
      Left g -> hoist go g
    {-# INLINE go #-}
  in
    go
{-# INLINE finalToFinal #-}

------------------------------------------------------------------------------
-- | Transform an @'Embed' m@ effect into a @'Final' m@ effect
--
-- @since 1.2.0.0
embedToFinal :: (Member (Final m) r, Monad m)
             => Sem (Embed m ': r) a
             -> Sem r a
embedToFinal = interpret $ \(Embed m) -> embedFinal m
{-# INLINE embedToFinal #-}
