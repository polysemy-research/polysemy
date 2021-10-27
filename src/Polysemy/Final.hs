{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Final
  (
    -- * Effect
    Final(..)
  , ThroughWeavingToFinal

    -- * Actions
  , withWeavingToFinal
  , withStrategicToFinal
  , embedFinal

    -- * Combinators for Interpreting to the Final Monad
  , interpretFinal

    -- * Strategy
    -- | Strategy is a domain-specific language very similar to @Tactics@
    -- (see 'Polysemy.Tactical'), and is used to describe how higher-order
    -- effects are threaded down to the final monad.
    --
    -- Much like @Tactics@, computations can be run and threaded
    -- through the use of 'runS' and 'bindS', and first-order constructors
    -- may use 'pureS'. In addition, 'liftS' may be used to
    -- lift actions of the final monad.
    --
    -- Unlike @Tactics@, the final return value within a 'Strategic'
    -- must be a monadic value of the target monad
    -- with the functorial state wrapped inside of it.
  , Strategic
  , WithStrategy
  , pureS
  , liftS
  , runS
  , bindS
  , getInspectorS
  , getInitialStateS

    -- * Interpretations
  , runFinal
  , finalToFinal

  -- * Interpretations for Other Effects
  , embedToFinal
  ) where

import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.Union
import Polysemy.Internal.Strategy
import Polysemy.Internal.TH.Effect

-----------------------------------------------------------------------------
-- | This represents a function which produces
-- an action of the final monad @m@ given:
--
--   * The initial effectful state at the moment the action
--     is to be executed.
--
--   * A way to convert @z@ (which is typically @'Sem' r@) to @m@ by
--     threading the effectful state through.
--
--   * An inspector that is able to view some value within the
--     effectful state if the effectful state contains any values.
--
-- A @'Polysemy.Internal.Union.Weaving'@ provides these components,
-- hence the name 'ThroughWeavingToFinal'.
--
-- @since 1.2.0.0
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
-- 'Final' is more powerful than 'Embed', but is also less flexible
-- to interpret (compare 'Polysemy.Embed.runEmbedded' with 'finalToFinal').
-- If you only need the power of 'embed', then you should use 'Embed' instead.
--
-- /Beware/: 'Final' actions are interpreted as actions of the final monad,
-- and the effectful state visible to
-- 'withWeavingToFinal' \/ 'withStrategicToFinal'
-- \/ 'interpretFinal'
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
--
-- @since 1.2.0.0
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
-- Consider using 'withStrategicToFinal' instead,
-- which provides a more user-friendly interface, but is also slightly weaker.
--
-- You are discouraged from using 'withWeavingToFinal' directly
-- in application code, as it ties your application code directly to
-- the final monad.
--
-- @since 1.2.0.0
withWeavingToFinal
  :: forall m r a
   . Member (Final m) r
  => ThroughWeavingToFinal m (Sem r) a
  -> Sem r a


-----------------------------------------------------------------------------
-- | 'withWeavingToFinal' admits an implementation of 'embed'.
--
-- Just like 'embed', you are discouraged from using this in application code.
--
-- @since 1.2.0.0
embedFinal :: (Member (Final m) r, Functor m) => m a -> Sem r a
embedFinal m = withWeavingToFinal $ \s _ _ -> (<$ s) <$> m
{-# INLINE embedFinal #-}

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad. This is done through the use of the 'Strategic'
-- environment, which provides 'runS' and 'bindS'.
--
-- You are discouraged from using 'withStrategicToFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @since 1.2.0.0
withStrategicToFinal :: Member (Final m) r
                     => Strategic m (Sem r) a
                     -> Sem r a
withStrategicToFinal strat = withWeavingToFinal (runStrategy strat)
{-# INLINE withStrategicToFinal #-}

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
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to effects
-- interpreted using 'interpretFinal'. See 'Final'.
--
-- @since 1.2.0.0
interpretFinal
    :: forall m e r a
     . Member (Final m) r
    => (forall x n. e n x -> Strategic m n x)
       -- ^ A natural transformation from the handled effect to the final monad.
    -> Sem (e ': r) a
    -> Sem r a
interpretFinal n =
  let
    go :: Sem (e ': r) x -> Sem r x
    go = hoistSem $ \u -> case decomp u of
      Right (Weaving e s wv ex ins) ->
        injWeaving $
          Weaving
            (WithWeavingToFinal (runStrategy (n e)))
            s
            (go . wv)
            ex
            ins
      Left g -> hoist go g
    {-# INLINE go #-}
  in
    go
{-# INLINE interpretFinal #-}

------------------------------------------------------------------------------
-- | Lower a 'Sem' containing only a single lifted, final 'Monad' into that
-- monad.
--
-- If you also need to process an @'Embed' m@ effect, use this together with
-- 'embedToFinal'.
--
-- @since 1.2.0.0
runFinal :: Monad m => Sem '[Final m] a -> m a
runFinal = usingSem $ \u -> case extract u of
  Weaving (WithWeavingToFinal wav) s wv ex ins ->
    ex <$> wav s (runFinal . wv) ins
{-# INLINE runFinal #-}

------------------------------------------------------------------------------
-- | Given natural transformations between @m1@ and @m2@, run a @'Final' m1@
-- effect by transforming it into a @'Final' m2@ effect.
--
-- @since 1.2.0.0
finalToFinal :: forall m1 m2 r a
              . Member (Final m2) r
             => (forall x. m1 x -> m2 x)
             -> (forall x. m2 x -> m1 x)
             -> Sem (Final m1 ': r) a
             -> Sem r a
finalToFinal to from =
  let
    go :: Sem (Final m1 ': r) x -> Sem r x
    go = hoistSem $ \u -> case decomp u of
      Right (Weaving (WithWeavingToFinal wav) s wv ex ins) ->
        injWeaving $
          Weaving
            (WithWeavingToFinal $ \s' wv' ins' ->
              to $ wav s' (from . wv') ins'
            )
            s
            (go . wv)
            ex
            ins
      Left g -> hoist go g
    {-# INLINE go #-}
  in
    go
{-# INLINE finalToFinal #-}

------------------------------------------------------------------------------
-- | Transform an @'Embed' m@ effect into a @'Final' m@ effect
--
-- @since 1.2.0.0
embedToFinal :: (Member (Final m) r, Functor m)
             => Sem (Embed m ': r) a
             -> Sem r a
embedToFinal = interpret $ \(Embed m) -> embedFinal m
{-# INLINE embedToFinal #-}
