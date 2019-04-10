{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE QuantifiedConstraints #-}

module Polysemy.Internal.Tactics
  ( Tactics (..)
  , getInitialStateT
  , runT
  , bindT
  , pureT
  , liftT
  , runTactics
  , Tactical
  , WithTactics
  ) where

import Polysemy.Internal
import Polysemy.Internal.Effect
import Polysemy.Internal.Union

type Tactical e m r x = ∀ f. (Functor f, Typeable1 f)
                          => Semantic (WithTactics e f m r) (f x)
type WithTactics e f m r = Tactics f m (e ': r) ': r

data Tactics f n r m a where
  GetInitialState     :: Tactics f n r m (f ())
  HoistInterpretation :: (a -> n b) -> Tactics f n r m (f a -> Semantic r (f b))


getInitialStateT :: forall f m r e. Semantic (WithTactics e f m r) (f ())
getInitialStateT = send @(Tactics _ m (e ': r)) GetInitialState


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
    -> Semantic (WithTactics e f m r)
                (Semantic (e ': r) (f a))
runT na = do
  istate <- getInitialStateT
  na'    <- bindT (const na)
  pure $ na' istate
{-# INLINE runT #-}


bindT
    :: (a -> m b)
       -- ^ The monadic continuation to lift. This is usually a parameter in
       -- your effect.
       --
       -- Continuations lifted via 'bindT' will run in the same environment
       -- which produced the 'a'.
    -> Semantic (WithTactics e f m r)
                (f a -> Semantic (e ': r) (f b))
bindT f = send $ HoistInterpretation f
{-# INLINE bindT #-}


liftT
    :: forall m f r e a
     . ( Functor f
       , Typeable1 f
       )
    => Semantic r a
    -> Semantic (WithTactics e f m r) (f a)
liftT m = do
  a <- raise m
  pureT a
{-# INLINE liftT #-}


runTactics
   :: Functor f
   => f ()
   -> (∀ x. f (m x) -> Semantic r2 (f x))
   -> Semantic (Tactics f m r2 ': r) a
   -> Semantic r a
runTactics s d (Semantic m) = m $ \u ->
  case decomp u of
    Left x -> liftSemantic $ hoist (runTactics_b s d) x
    Right (Yo GetInitialState s' _ y) ->
      pure $ y $ s <$ s'
    Right (Yo (HoistInterpretation na) s' _ y) -> do
      pure $ y $ (d . fmap na) <$ s'
{-# INLINE runTactics #-}


runTactics_b
   :: Functor f
   => f ()
   -> (∀ x. f (m x) -> Semantic r2 (f x))
   -> Semantic (Tactics f m r2 ': r) a
   -> Semantic r a
runTactics_b = runTactics
{-# NOINLINE runTactics_b #-}

