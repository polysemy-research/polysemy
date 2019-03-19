{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Definitive
  ( module Definitive
  , module Definitive.Effect
  , module Definitive.Lift
  , Member
  , decomp
  , prj
  ) where

import           Control.Monad (join)
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Functor.Identity
import           Data.Tuple
import           Definitive.Effect
import           Definitive.Lift
import           Definitive.Union

type Def r = Freer (Union r)

newtype Freer f a = Freer
  { runFreer
        :: ∀ m
         . Monad m
        => (∀ x. f (Freer f) x -> m x)
        -> m a
  }

usingFreer :: Monad m => (∀ x. f (Freer f) x -> m x) -> Freer f a -> m a
usingFreer k m = runFreer m k
{-# INLINE usingFreer #-}


instance Functor (Freer f) where
  fmap f (Freer m) = Freer $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Freer f) where
  pure a = Freer $ const $ pure a
  {-# INLINE pure #-}

  Freer f <*> Freer a = Freer $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Freer f) where
  return = pure
  {-# INLINE return #-}

  Freer ma >>= f = Freer $ \k -> do
    z <- ma k
    runFreer (f z) k
  {-# INLINE (>>=) #-}


liftEff :: f (Freer f) a -> Freer f a
liftEff u = Freer $ \k -> k u
{-# INLINE liftEff #-}


hoistEff :: (∀ x. f (Freer f) x -> g (Freer g) x) -> Freer f a -> Freer g a
hoistEff nat (Freer m) = Freer $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistEff #-}


send :: Member e r => e (Def r) a -> Def r a
send = liftEff . inj
{-# INLINE[3] send #-}


sendM :: Member (Lift m) r => m a -> Def r a
sendM = send . Lift
{-# INLINE sendM #-}


run :: Def '[] a -> a
run (Freer m) = runIdentity $ m absurdU
{-# INLINE run #-}


runM :: Monad m => Def '[Lift m] a -> m a
runM (Freer m) = m $ unLift . extract
{-# INLINE runM #-}


interpret
    :: Effect e
    => (∀ x. e (Def (e ': r)) x -> Def r x)
    -> Def (e ': r) a
    -> Def r a
interpret f (Freer m) = m $ \u ->
  case decomp u of
    Left  x -> liftEff $ hoist (interpret f) x
    Right y -> f y
{-# INLINE interpret #-}


stateful
    :: Effect e
    => (∀ x. e (Def (e ': r)) x -> StateT s (Def r) x)
    -> s
    -> Def (e ': r) a
    -> Def r (s, a)
stateful f s (Freer m) = Freer $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ stateful' f)
            $ x
        Right y -> S.mapStateT (usingFreer k) $ f y
{-# INLINE stateful #-}


stateful'
    :: Effect e
    => (∀ x. e (Def (e ': r)) x -> StateT s (Def r) x)
    -> s
    -> Def (e ': r) a
    -> Def r (s, a)
stateful' = stateful
{-# NOINLINE stateful' #-}


reinterpret
    :: Effect f
    => (∀ x. f (Def (f ': r)) x -> Def (g ': r) x)
    -> Def (f ': r) a
    -> Def (g ': r) a
reinterpret f (Freer m) = Freer $ \k -> m $ \u ->
  case prjCoerce u of
    Left x -> k $ hoist (reinterpret' f) $ x
    Right y  -> usingFreer k $ f y
{-# INLINE[3] reinterpret #-}


reinterpret'
    :: Effect f
    => (∀ x. f (Def (f ': r)) x -> Def (g ': r) x)
    -> Def (f ': r) a
    -> Def (g ': r) a
reinterpret' = reinterpret
{-# NOINLINE reinterpret' #-}


runRelayS
    :: Effect e
    => (∀ x. e (Def (e ': r)) x -> s -> Def r (s, x))
    -> s
    -> Def (e ': r) a
    -> Def r (s, a)
runRelayS f = stateful $ \e -> S.StateT $ fmap swap . f e
{-# INLINE runRelayS #-}

