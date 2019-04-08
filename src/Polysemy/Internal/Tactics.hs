{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.Internal.Tactics
  ( Tactics (..)
  , start
  , continue
  , begin
  , toH2
  , runTactics
  , Tactical
  ) where

import Polysemy
import Polysemy.Internal.Union
import Polysemy.Internal.Effect

type Tactical e m r x = ∀ f. Functor f => Semantic (Tactics f m (e ': r) ': r) (f x)

data Tactics f n r m a where
  GetInitialState     :: Tactics f n r m (f ())
  HoistInterpretation :: (a -> n b) -> Tactics f n r m (f a -> Semantic r (f b))

begin
    :: forall n r a e. a -> Tactical e n r a
begin a = do
  istate <- send @(Tactics _ n (e ': r)) GetInitialState
  pure $ a <$ istate

start
    :: forall f n r a e
     . n a
    -> Semantic (Tactics f n (e ': r) ': r) (Semantic (e ': r) (f a))
start na = do
  istate <- send @(Tactics _ n (e ': r)) GetInitialState
  na'    <- continue (const na)
  pure $ na' istate
{-# INLINE start #-}


continue :: (a -> n b) -> Semantic (Tactics f n (e ': r) ': r) (f a -> Semantic (e ': r) (f b))
continue f = send $ HoistInterpretation f
{-# INLINE continue #-}


toH2
    :: forall n f r a e
     . ( Functor f
       )
    => Semantic r a
    -> Semantic (Tactics f n (e ': r) ': r) (f a)
toH2 m = do
  istate <- send @(Tactics f n (e ':r)) GetInitialState
  raise $ fmap (<$ istate) m
{-# INLINE toH2 #-}


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

