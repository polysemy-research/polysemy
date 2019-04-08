{-# LANGUAGE AllowAmbiguousTypes #-}

module Polysemy.Tactics.Type
  ( Tactics (..)
  , start
  , continue
  , toH
  , toH2
  , runTactics
  ) where

import Polysemy
import Polysemy.Union
import Polysemy.Effect

data Tactics f n r m a where
  GetInitialState     :: Tactics f n r m (f ())
  HoistInterpretation :: (a -> n b) -> Tactics f n r m (f a -> Semantic r (f b))

start
    :: forall f n r r' a
     . Member (Tactics f n r) r'
    => n a
    -> Semantic r' (Semantic r (f a))
start na = do
  istate <- send @(Tactics _ n r) GetInitialState
  na'    <- continue (const na)
  pure $ na' istate
{-# INLINE start #-}


continue :: Member (Tactics f n r) r' => (a -> n b) -> Semantic r' (f a -> Semantic r (f b))
continue f = send $ HoistInterpretation f
{-# INLINE continue #-}


toH
    :: forall n f r r' a e
     . ( Functor f
       , r' ~ (e : r)
       , Member (Tactics f n r) r'
       )
    => Semantic r a
    -> Semantic r' (f a)
toH m = do
  istate <- send @(Tactics f n r) GetInitialState
  raise $ fmap (<$ istate) m
{-# INLINE toH #-}

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

