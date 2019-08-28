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
    -- (see 'Tactical'), and is used to describe how higher-order effects
    -- are threaded down to the final monad.
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

import Control.Monad

import Data.Functor.Compose

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.Final

-----------------------------------------------------------------------------
-- | 'withWeavingToFinal' admits an implementation of 'embed'.
--
-- Just like 'embed', you are discouraged from using this in application code.
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
withStrategicToFinal :: Member (Final m) r
                     => Strategic m (Sem r) a
                     -> Sem r a
withStrategicToFinal strat = withWeavingToFinal $ \s wv ins ->
  runStrategy
    s
    wv
    ins
    strat
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
interpretFinal
    :: forall m e r a
     . (Member (Final m) r, Functor m)
    => (forall x n. e n x -> Strategic m n x)
       -- ^ A natural transformation from the handled effect to the final monad.
    -> Sem (e ': r) a
    -> Sem r a
interpretFinal n =
  let
    go :: Sem (e ': r) x -> Sem r x
    go = usingSem $ \u -> case decomp u of
      Right (Weaving e s wv ex ins) ->
        fmap ex $ withWeavingToFinal $ \s' wv' ins'
          -> fmap getCompose $
                runStrategy
                  (Compose (s <$ s'))
                  (fmap Compose . wv' . fmap (go . wv) . getCompose)
                  (ins' . getCompose >=> ins)
                  (n e)
      Left g -> liftSem (hoist go g)
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
runFinal :: Monad m => Sem '[Final m] a -> m a
runFinal = usingSem $ \u -> case extract u of
  Weaving (WithWeavingToFinal wav) s wv ex ins ->
    ex <$> wav s (runFinal . wv) ins
{-# INLINE runFinal #-}

------------------------------------------------------------------------------
-- | Given natural transformations between @m1@ and @m2@, run a @'Final' m1@
-- effect by transforming it into a @'Final' m2@ effect.
finalToFinal :: forall m1 m2 a r
              . (Member (Final m2) r, Functor m2)
             => (forall x. m1 x -> m2 x)
             -> (forall x. m2 x -> m1 x)
             -> Sem (Final m1 ': r) a
             -> Sem r a
finalToFinal to from =
  let
    go :: Sem (Final m1 ': r) x -> Sem r x
    go = usingSem $ \u -> case decomp u of
      Right (Weaving (WithWeavingToFinal wav) s wv ex ins) ->
        fmap ex $ withWeavingToFinal $ \s' wv' ins'
          -> fmap getCompose $ to $
                wav
                  (Compose (s <$ s'))
                  (from . fmap Compose . wv' . fmap (go . wv) . getCompose)
                  (ins' . getCompose >=> ins)
      Left g -> liftSem (hoist go g)
    {-# INLINE go #-}
  in
    go
{-# INLINE finalToFinal #-}

------------------------------------------------------------------------------
-- | Transform an @'Embed' m@ effect into a @'Final' m@ effect
embedToFinal :: (Member (Final m) r, Functor m)
             => Sem (Embed m ': r) a
             -> Sem r a
embedToFinal = interpret $ \(Embed m) -> embedFinal m
{-# INLINE embedToFinal #-}
