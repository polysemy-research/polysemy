{-# OPTIONS_HADDOCK not-home #-}
module Polysemy.Internal.Strategy where

import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.Tactics (Inspector(..))


data Strategy m f n z a where
  GetInitialState     :: Strategy m f n z (f ())
  HoistInterpretation :: (a -> n b) -> Strategy m f n z (f a -> m (f b))
  GetInspector        :: Strategy m f n z (Inspector f)

------------------------------------------------------------------------------
-- | 'Strategic' is an environment in which you're capable of explicitly
-- threading higher-order effect states to the final monad.
-- This is a variant of @Tactics@ (see 'Polysemy.Tactical'), and usage
-- is extremely similar.
type Strategic m n a = forall f. Functor f => Sem (WithStrategy m f n) (m (f a))

type WithStrategy m f n = '[Strategy m f n]

------------------------------------------------------------------------------
-- | Internal function to process Strategies in terms of
-- 'Polysemy.Final.withWeavingToFinal'.
runStrategy :: Functor f
            => f ()
            -> (forall x. f (n x) -> m (f x))
            -> (forall x. f x -> Maybe x)
            -> Sem '[Strategy m f n] a
            -> a
runStrategy s wv ins = (run .) $ interpret $ \case
  GetInitialState       -> pure s
  HoistInterpretation f -> pure $ \fa -> wv (f <$> fa)
  GetInspector          -> pure (Inspector ins)
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
getInspectorS :: forall m f n. Sem (WithStrategy m f n) (Inspector f)
getInspectorS = send (GetInspector @m @f @n)
{-# INLINE getInspectorS #-}

------------------------------------------------------------------------------
-- | Get the stateful environment of the world at the moment the
-- target monad is to be run.
-- Prefer 'pureS', 'liftS', 'runS', or 'bindS' instead of using this function
-- directly.
getInitialStateS :: forall m f n. Sem (WithStrategy m f n) (f ())
getInitialStateS = send (GetInitialState @m @f @n)
{-# INLINE getInitialStateS #-}

------------------------------------------------------------------------------
-- | Embed a value into 'Strategic'.
pureS :: Applicative m => a -> Strategic m n a
pureS a = pure . (a <$) <$> getInitialStateS
{-# INLINE pureS #-}

------------------------------------------------------------------------------
-- | Lifts an action of the final monad into 'Strategic'.
--
-- Note: you don't need to use this function if you already have a monadic
-- action with the functorial state woven into it, by the use of
-- 'runS' or 'bindS'.
-- In these cases, you need only use 'pure' to embed the action into the
-- 'Strategic' environment.
liftS :: Functor m => m a -> Strategic m n a
liftS m = do
  s <- getInitialStateS
  pure $ fmap (<$ s) m
{-# INLINE liftS #-}

------------------------------------------------------------------------------
-- | Lifts a monadic action into the stateful environment, in terms
-- of the final monad.
-- The stateful environment will be the same as the one that the target monad
-- is initially run in.
-- Use 'bindS'  if you'd prefer to explicitly manage your stateful environment.
runS :: n a -> Sem (WithStrategy m f n) (m (f a))
runS na = bindS (const na) <*> getInitialStateS
{-# INLINE runS #-}

------------------------------------------------------------------------------
-- | Embed a kleisli action into the stateful environment, in terms of the final
-- monad. You can use 'bindS' to get an effect parameter of the form @a -> n b@
-- into something that can be used after calling 'runS' on an effect parameter
-- @n a@.
bindS :: (a -> n b) -> Sem (WithStrategy m f n) (f a -> m (f b))
bindS = send . HoistInterpretation
{-# INLINE bindS #-}
