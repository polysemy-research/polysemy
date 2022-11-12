{-# LANGUAGE TemplateHaskell, PatternGuards, EmptyCase #-}
module Polysemy.Internal.Final
  (
    -- * Effect
    Final(..)

    -- * Actions
  , withStrategicToFinal
  , withLoweringToFinal
  , controlF
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
  , controlS
  , liftWithS
  , restoreS
  , runS
  , controlS'
  , liftWithS'

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
import Polysemy.Internal.Interpretation (interpret)

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
-- and the effectful state visible to 'controlF' \/ 'withStrategicToFinal'
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
  WithLoweringToFinal
    :: (forall t. MonadTransWeave t => (forall x. z x -> t m x) -> t m a)
    -> Final m z a

makeSem ''Final

data Strategy m t n z a where
  LiftWithS :: forall m t n z a
             . (  (  forall x
                   . Sem '[Strategy m t n, Final m, Embed m] x
                  -> m (t x)
                  )
               -> m a
               )
            -> Strategy m t n z a
  RestoreS  :: forall m t n z a. t a -> Strategy m t n z a
  RunS      :: forall m t n z a. n a -> Strategy m t n z a

restoreS :: forall m t n r a. t a -> Sem (Strategy m t n ': r) a
restoreS = send . RestoreS @m @_ @n
{-# INLINE restoreS #-}

runS :: forall m t n r a. n a -> Sem (Strategy m t n ': r) a
runS = send . RunS @m @t
{-# INLINE runS #-}

liftWithS' :: forall m t n r a
            . (  (  forall x
                  . Sem '[Strategy m t n, Final m, Embed m] x -> m (t x)
                 )
              -> m a
              )
           -> Sem (Strategy m t n ': r) a
liftWithS' main = send (LiftWithS main)
{-# INLINE liftWithS' #-}

controlS' :: forall m t n r a
           . (  (  forall x
                 . Sem '[Strategy m t n, Final m, Embed m] x -> m (t x)
                )
             -> m (t a)
             )
          -> Sem (Strategy m t n ': r) a
controlS' main = liftWithS' main >>= restoreS
{-# INLINE controlS' #-}

liftWithS :: forall m t n r a
           . ((forall x. n x -> m (t x)) -> m a) -> Sem (Strategy m t n ': r) a
liftWithS main = liftWithS' $ \n -> main (n . runS)
{-# INLINE liftWithS #-}

controlS :: forall m t n r a
          . ((forall x. n x -> m (t x)) -> m (t a))
         -> Sem (Strategy m t n ': r) a
controlS main = controlS' $ \n -> main (n . runS)
{-# INLINE controlS #-}

-- | A convenience method for @'withStrategicToFinal' . 'controlS'@
controlF :: forall m r a
          . (Member (Final m) r, Monad m)
         => (  forall t
             . Traversable t
            => (forall x. Sem r x -> m (t x)) -> m (t a)
            )
         -> Sem r a
controlF main = withLoweringToFinal $ \n ->
  controlT $ \lower -> main (lower . n)
{-# INLINE controlF #-}

type Strategic m n a =
  forall t. Traversable t => Sem '[Strategy m t n, Final m, Embed m] a

runStrategy :: forall m n t a
             . (Monad m, MonadTransWeave t)
            => Sem '[Strategy m (StT t) n, Final m, Embed m] a
            -> (forall x. n x -> t m x) -> t m a
runStrategy main nat =
  let
    go :: forall x. Sem '[Strategy m (StT t) n, Final m, Embed m] x -> t m x
    go = usingSem $ \(Union pr (Weaving eff mkT lwr ex)) -> do
      let run_it = (ex . (<$ mkInitState lwr))
      case pr of
        Here -> run_it <$> case eff of
          RestoreS t -> restoreT (return t)
          RunS m -> nat m
          LiftWithS main' -> liftWith $ \lower -> main' (lower . go)
        There Here | WithLoweringToFinal main' <- eff ->
          fmap ex $ lwr $ getComposeT $ main' (ComposeT . mkT go)
        There (There Here) | Embed m <- eff -> run_it <$> lift m
        There (There (There pr_)) -> case pr_ of {}
  in
    go main

-----------------------------------------------------------------------------
-- | Allows for embedding higher-order actions of the final monad
-- by providing the means of explicitly threading effects through @'Sem' r@
-- to the final monad. This is done through the use of the 'Strategic'
-- environment, which provides a variety of combinators, most notably 'controlS'.
--
-- You are discouraged from using 'withStrategicToFinal' in application code,
-- as it ties your application code directly to the final monad.
--
-- @since 1.2.0.0
withStrategicToFinal :: (Monad m, Member (Final m) r)
                     => Strategic m (Sem r) a
                     -> Sem r a
withStrategicToFinal main = withLoweringToFinal (runStrategy main)
{-# INLINE withStrategicToFinal #-}

------------------------------------------------------------------------------
-- | Lower a 'Sem' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Sem '[Final m, Embed m] a -> m a
runM = usingSem $ \u -> case decomp u of
  Right (Weaving (WithLoweringToFinal main) mkT lwr ex) ->
    fmap ex $ lwr $ main $ mkT runM
  Left g -> case extract g of
    Weaving (Embed m) _ lwr ex -> fmap (ex . (<$ mkInitState lwr)) m
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
-- /Beware/: Effects that aren't interpreted in terms of the final
-- monad will have local state semantics in regards to effects
-- interpreted using 'interpretFinal'. See 'Final'.
--
-- @since 1.2.0.0
interpretFinal
    :: forall m e r a
     . (Member (Final m) r, Monad m)
    => (forall x rInitial. e (Sem rInitial) x -> Strategic m (Sem rInitial) x)
       -- ^ A natural transformation from the handled effect to the final monad.
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
