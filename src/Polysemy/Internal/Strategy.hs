{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Strategy where

import Polysemy.Final.Type
import Polysemy.Internal
import Polysemy.Internal.Union
import Polysemy.Internal.Tactics


------------------------------------------------------------------------------
-- | 'Strategic' is an environment in which you're capable of explicitly
-- threading higher-order effect states to the final monad.
-- This is a variant of @Tactics@ (see 'Polysemy.Tactical'), and usage
-- is extremely similar.
--
-- @since 1.2.0.0
type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (f a)


------------------------------------------------------------------------------
-- | @since 1.2.0.0
type WithStrategy m f n = '[Tactics m f n, Embed m, Final m]


------------------------------------------------------------------------------
-- | Internal function to process Strategies in terms of
-- 'Polysemy.Final.withWeavingToFinal'.
--
-- @since 1.2.0.0
runStrategy :: forall m f n a
             . (Functor f, Monad m)
            => Sem '[Tactics m f n, Embed m, Final m] a
            -> f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> m a
runStrategy sem = \s wv ins ->
  let
    go :: forall x. Sem '[Tactics m f n, Embed m, Final m] x -> m x
    go = usingSem $ \u ->
     case decomp u of
       Right (Weaving e s' _ ex _) ->
         let pure' a = pure @m (ex (a <$ s'))
         in case e of
          GetInitialState -> pure' s
          HoistInterpretation f -> pure' (\fa -> wv (f <$> fa))
          GetInspector -> pure' (Inspector ins)
       Left g -> case decomp g of
        Right (Weaving (Embed m') s' _ ex _) ->
          (ex . (<$ s')) <$> m'
        Left g' -> case extract g' of
          Weaving (WithWeavingToFinal wav) s' wv' ex ins' ->
            ex <$> wav s' (go . wv') ins'
  in
    go sem
  -- (\case
  --   GetInitialState       -> pure s
  --   HoistInterpretation f -> pure $ \fa -> wv (f <$> fa)
  --   GetInspector          -> pure (Inspector ins)
  -- ) $ sem
{-# INLINE runStrategy #-}


------------------------------------------------------------------------------
-- | Get a natural transformation capable of potentially inspecting values
-- inside of @f@. Binding the result of 'getInspectorS' produces a function that
-- can sometimes peek inside values returned by 'bindS'.
--
-- This is often useful for running callback functions that are not managed by
-- polysemy code.
--
-- See also 'Polysemy.getInspectorT'
--
-- @since 1.2.0.0
getInspectorS :: forall m f n. Sem (WithStrategy m f n) (Inspector f)
getInspectorS = getInspectorT @m @n
{-# INLINE getInspectorS #-}


------------------------------------------------------------------------------
-- | Get the stateful environment of the world at the moment the
-- @Strategy@ is to be run.
--
-- Prefer 'pureS', 'liftS', 'runS', or 'bindS' instead of using this function
-- directly.
--
-- @since 1.2.0.0
getInitialStateS :: forall m f n. Sem (WithStrategy m f n) (f ())
getInitialStateS = getInitialStateT @m @n
{-# INLINE getInitialStateS #-}


------------------------------------------------------------------------------
-- | Embed a value into 'Strategic'.
--
-- @since 1.2.0.0
pureS :: Applicative m => a -> Strategic m n a
pureS a = (a <$) <$> getInitialStateS
{-# INLINE pureS #-}


------------------------------------------------------------------------------
-- | Lifts an action of the final monad into 'Strategic'.
--
-- /Note/: you don't need to use this function if you already have a monadic
-- action with the functorial state threaded into it, by the use of
-- 'runS' or 'bindS'.
-- In these cases, you need only use 'pure' to embed the action into the
-- 'Strategic' environment.
--
-- @since 1.2.0.0
liftS :: Functor m => m a -> Strategic m n a
liftS m = do
  s <- getInitialStateS
  embed $ fmap (<$ s) m
{-# INLINE liftS #-}


------------------------------------------------------------------------------
-- | Lifts a monadic action into the stateful environment, in terms
-- of the final monad.
-- The stateful environment will be the same as the one that the @Strategy@
-- is initially run in.
--
-- Use 'bindS'  if you'd prefer to explicitly manage your stateful environment.
--
-- @since 1.2.0.0
runS :: n a -> Sem (WithStrategy m f n) (m (f a))
runS = runT
{-# INLINE runS #-}


------------------------------------------------------------------------------
-- | Embed a kleisli action into the stateful environment, in terms of the final
-- monad. You can use 'bindS' to get an effect parameter of the form @a -> n b@
-- into something that can be used after calling 'runS' on an effect parameter
-- @n a@.
--
-- @since 1.2.0.0
bindS :: (a -> n b) -> Sem (WithStrategy m f n) (f a -> m (f b))
bindS = bindT
{-# INLINE bindS #-}

