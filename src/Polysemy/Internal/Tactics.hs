{-# LANGUAGE AllowAmbiguousTypes #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Tactics
  ( Tactics (..)
  , getInitialStateT
  , getInspectorT
  , Inspector (..)
  , runT
  , runTSimple
  , bindT
  , bindTSimple
  , pureT
  , liftT
  , runTactics
  , Tactical
  , WithTactics
  ) where

import Polysemy.Internal
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | 'Tactical' is an environment in which you're capable of explicitly
-- threading higher-order effect states. This is provided by the (internal)
-- effect @Tactics@, which is capable of rewriting monadic actions so they run
-- in the correct stateful environment.
--
-- The @f@ type here is existential and corresponds to "whatever
-- state the other effects want to keep track of." @f@ is always
-- a 'Functor'.
--
-- Inside a 'Tactical', you're capable of running 'pureT', 'runTSimple', 'bindTSimple',
-- 'runT' and 'bindT', which are the main tools for rewriting monadic stateful environments.
--
-- You should usually reach for 'runTSimple' and 'bindTSimple' first, as they are easier to use,
-- albeit less powerful versions of 'runT' and 'bindT'.
--
-- For example, consider trying to write an interpreter for
-- 'Polysemy.Resource.Resource', whose effect is defined as:
--
-- @
-- data 'Polysemy.Resource.Resource' m a where
--   'Polysemy.Resource.Bracket' :: m a -> (a -> m ()) -> (a -> m b) -> 'Polysemy.Resource.Resource' m b
-- @
-- We can interpret this effect like so:
--
-- @
-- runResource = Polysemy.interpretH $ \\case
--  'Polysemy.Resource.Bracket' alloc dealloc use -> do
--    resource <- 'runTSimple' alloc
--    result <- 'bindTSimple' use resource
--    'bindTSimple' dealloc resource
--    pure result
-- @
--
-- To run embedded higher-order actions (e.g. @alloc :: m a@s) we use 'runTSimple'.
--
-- If we have a kleisli action (e.g. @use :: a -> m b@) we can instead use 'bindTSimple'.
--
-- Note that because of the types of @'bindTSimple' use resource@ and @'bindTSimple' dealloc resource@
-- both use @f a@, they must run in the same stateful environment. This
-- means, for illustration, any 'Polysemy.State.put's run inside the @'bindTSimple' use resource@
-- block will not be visible inside of the @dealloc@ block.
--
-- There are however some cases where the @\*Simple@ functions are not sufficient.
--
-- Consider, for example, the 'Polysemy.Reader.local' action.
--
-- In it, we want any inner computations to be executed with a __modified__ environment.
--
-- This is not possible with 'runTSimple' and 'bindTSimple', because they automatically
-- take the care to run all your embedded actions by using the current set of interpreters.
--
-- Instead, we need to use 'runT' and 'bindT', which give more control over how inner actions are run.
--
-- As an example, let's look at the implementation for running a 'Polysemy.Reader.Reader'
-- with a constant value:
--
-- @
-- 'Polysemy.Reader.runReader' i = 'Polysemy.interpretH' $ \\case
--   'Polysemy.Reader.Ask' -> pureT i
--   'Polysemy.Reader.Local' f m -> do
--     mm <- 'runT' m
--     'Polysemy.raise' $ 'Polysemy.Reader.runReader' (f i) mm
-- @
--
-- We can see that there is a recursive call to 'Polysemy.Reader.runReader', which handles
-- any other potential 'Polysemy.Reader' computations that might be present in our @m@.
--
-- The thing to note here is that the call is made with a __modified__ environment of @f i@
-- instead of the __initial__ @i@ that our interpreter was currently running with.
type Tactical e m r x = ∀ f. Functor f
                          => Sem (WithTactics e f m r) (f x)

type WithTactics e f m r = Tactics f m (e ': r) ': r

data Tactics f n r m a where
  GetInitialState      :: Tactics f n r m (f ())
  HoistInterpretation  :: (a -> n b) -> Tactics f n r m (f a -> Sem r (f b))
  HoistInterpretationH :: (a -> n b) -> f a -> Tactics f n r m (f b)
  GetInspector         :: Tactics f n r m (Inspector f)


------------------------------------------------------------------------------
-- | Get the stateful environment of the world at the moment the effect @e@ is
-- to be run. Prefer 'pureT', 'runT' or 'bindT' instead of using this function
-- directly.
getInitialStateT :: forall f m r e. Sem (WithTactics e f m r) (f ())
getInitialStateT = send @(Tactics _ m (e ': r)) GetInitialState


------------------------------------------------------------------------------
-- | Get a natural transformation capable of potentially inspecting values
-- inside of @f@. Binding the result of 'getInspectorT' produces a function that
-- can sometimes peek inside values returned by 'bindT'.
--
-- This is often useful for running callback functions that are not managed by
-- polysemy code.
--
-- ==== Example
--
-- We can use the result of 'getInspectorT' to "undo" 'pureT' (or any of the other
-- 'Tactical' functions):
--
-- @
-- ins <- 'getInspectorT'
-- fa <- 'pureT' "hello"
-- fb <- 'pureT' True
-- let a = 'inspect' ins fa   -- Just "hello"
--     b = 'inspect' ins fb   -- Just True
-- @
getInspectorT :: forall e f m r. Sem (WithTactics e f m r) (Inspector f)
getInspectorT = send @(Tactics _ m (e ': r)) GetInspector


------------------------------------------------------------------------------
-- | A container for 'inspect'. See the documentation for 'getInspectorT'.
newtype Inspector f = Inspector
  { inspect :: forall x. f x -> Maybe x
    -- ^ See the documentation for 'getInspectorT'.
  }


------------------------------------------------------------------------------
-- | Lift a value into 'Tactical'.
pureT :: a -> Tactical e m r a
pureT a = do
  istate <- getInitialStateT
  pure $ a <$ istate


------------------------------------------------------------------------------
-- | Run a monadic action in a 'Tactical' environment. The stateful environment
-- used will be the same one that the effect is initally run in. Use 'bindT' if
-- you'd prefer to explicitly manage your stateful environment.
runT
    :: m a
      -- ^ The monadic action to lift. This is usually a parameter in your
      -- effect.
    -> Sem (WithTactics e f m r)
                (Sem (e ': r) (f a))
runT na = do
  istate <- getInitialStateT
  na'    <- bindT (const na)
  pure $ na' istate
{-# INLINE runT #-}

------------------------------------------------------------------------------
-- | Run a monadic action in a 'Tactical' environment. The stateful environment
-- used will be the same one that the effect is initally run in.
-- Use 'bindTSimple' if you'd prefer to explicitly manage your stateful
-- environment.
--
-- This is a less flexible but significantly simpler variant of 'runT'.
-- Instead of returning a 'Sem' action corresponding to the provided action,
-- 'runTSimple' runs the action immediately.
--
-- @since TODO
runTSimple :: m a
              -- ^ The monadic action to lift. This is usually a parameter in your
              -- effect.
           -> Tactical e m r a
runTSimple na = do
  istate <- getInitialStateT
  bindTSimple (const na) istate
{-# INLINE runTSimple #-}


------------------------------------------------------------------------------
-- | Lift a kleisli action into the stateful environment. You can use
-- 'bindT' to get an effect parameter of the form @a -> m b@ into something
-- that can be used after calling 'runT' on an effect parameter @m a@.
bindT
    :: (a -> m b)
       -- ^ The monadic continuation to lift. This is usually a parameter in
       -- your effect.
       --
       -- Continuations lifted via 'bindT' will run in the same environment
       -- which produced the @a@.
    -> Sem (WithTactics e f m r)
                (f a -> Sem (e ': r) (f b))
bindT f = send $ HoistInterpretation f
{-# INLINE bindT #-}

------------------------------------------------------------------------------
-- | Lift a kleisli action into the stateful environment.
-- You can use 'bindTSimple' to execute an effect parameter of the form
-- @a -> m b@ by providing the result of a `runTSimple` or another
-- `bindTSimple`.
--
-- This is a less flexible but significantly simpler variant of 'bindT'.
-- Instead of returning a 'Sem' kleisli action corresponding to the
-- provided kleisli action, 'bindTSimple' runs the kleisli action immediately.
--
-- @since TODO
bindTSimple
    :: forall m f r e a b
     . (a -> m b)
       -- ^ The monadic continuation to lift. This is usually a parameter in
       -- your effect.
       --
       -- Continuations executed via 'bindTSimple' will run in the same
       -- environment which produced the @a@.
    -> f a
    -> Sem (WithTactics e f m r) (f b)
bindTSimple f s = send @(Tactics _ _ (e ': r)) $ HoistInterpretationH f s
{-# INLINE bindTSimple #-}


------------------------------------------------------------------------------
-- | Internal function to create first-order interpreter combinators out of
-- higher-order ones.
liftT
    :: forall m f r e a
     . Functor f
    => Sem r a
    -> Sem (WithTactics e f m r) (f a)
liftT m = do
  a <- raise m
  pureT a
{-# INLINE liftT #-}


------------------------------------------------------------------------------
-- | Run the 'Tactics' effect.
runTactics
   :: Functor f
   => f ()
   -> (∀ x. f (m x) -> Sem r2 (f x))
   -> (∀ x. f x -> Maybe x)
   -> (∀ x. f (m x) -> Sem r (f x))
   -> Sem (Tactics f m r2 ': r) a
   -> Sem r a
runTactics s d v d' (Sem m) = Sem $ \k -> m $ \u ->
  case decomp u of
    Left x -> k $ hoist (runTactics s d v d') x
    Right (Weaving GetInitialState s' _ y _) ->
      pure $ y $ s <$ s'
    Right (Weaving (HoistInterpretation na) s' _ y _) -> do
      pure $ y $ (d . fmap na) <$ s'
    Right (Weaving (HoistInterpretationH na fa) s' _ y _) -> do
      (y . (<$ s')) <$> runSem (d' (fmap na fa)) k
    Right (Weaving GetInspector s' _ y _) -> do
      pure $ y $ Inspector v <$ s'
{-# INLINE runTactics #-}

