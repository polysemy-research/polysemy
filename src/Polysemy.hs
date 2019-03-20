{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}

module Polysemy
  ( module Polysemy
  , module Polysemy.Effect
  , module Polysemy.Lift
  , Member
  , decomp
  , prj
  ) where

import           Control.Monad (join)
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Functor.Identity
import           Data.Tuple
import           Polysemy.Effect
import           Polysemy.Lift
import           Polysemy.Union


newtype Poly r a = Poly
  { runPoly
        :: ∀ m
         . Monad m
        => (∀ x. Union r (Poly r) x -> m x)
        -> m a
  }

usingPoly :: Monad m => (∀ x. Union r (Poly r) x -> m x) -> Poly r a -> m a
usingPoly k m = runPoly m k
{-# INLINE usingPoly #-}


instance Functor (Poly f) where
  fmap f (Poly m) = Poly $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Poly f) where
  pure a = Poly $ const $ pure a
  {-# INLINE pure #-}

  Poly f <*> Poly a = Poly $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Poly f) where
  return = pure
  {-# INLINE return #-}

  Poly ma >>= f = Poly $ \k -> do
    z <- ma k
    runPoly (f z) k
  {-# INLINE (>>=) #-}


instance (Member (Lift IO) r) => MonadIO (Poly r) where
  liftIO = sendM
  {-# INLINE liftIO #-}

instance MonadFix (Poly '[]) where
  mfix f = a
    where
      a = f (run a)


liftPoly :: Union r (Poly r) a -> Poly r a
liftPoly u = Poly $ \k -> k u
{-# INLINE liftPoly #-}


hoistPoly :: (∀ x. Union r (Poly r) x -> Union r' (Poly r') x) -> Poly r a -> Poly r' a
hoistPoly nat (Poly m) = Poly $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistPoly #-}


send :: Member e r => e (Poly r) a -> Poly r a
send = liftPoly . inj
{-# INLINE[3] send #-}


sendM :: Member (Lift m) r => m a -> Poly r a
sendM = send . Lift
{-# INLINE sendM #-}


run :: Poly '[] a -> a
run (Poly m) = runIdentity $ m absurdU
{-# INLINE run #-}


runM :: Monad m => Poly '[Lift m] a -> m a
runM (Poly m) = m $ unLift . extract
{-# INLINE runM #-}


interpret
    :: Effect e
    => (∀ x. e (Poly (e ': r)) x -> Poly r x)
    -> Poly (e ': r) a
    -> Poly r a
interpret f (Poly m) = m $ \u ->
  case decomp u of
    Left  x -> liftPoly $ hoist (interpret f) x
    Right y -> f y
{-# INLINE interpret #-}


stateful
    :: Effect e
    => (∀ x. e (Poly (e ': r)) x -> StateT s (Poly r) x)
    -> s
    -> Poly (e ': r) a
    -> Poly r (s, a)
stateful f s (Poly m) = Poly $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ stateful' f)
            $ x
        Right y -> S.mapStateT (usingPoly k) $ f y
{-# INLINE stateful #-}


stateful'
    :: Effect e
    => (∀ x. e (Poly (e ': r)) x -> StateT s (Poly r) x)
    -> s
    -> Poly (e ': r) a
    -> Poly r (s, a)
stateful' = stateful
{-# NOINLINE stateful' #-}


reinterpret
    :: Effect f
    => (∀ x. f (Poly (f ': r)) x -> Poly (g ': r) x)
    -> Poly (f ': r) a
    -> Poly (g ': r) a
reinterpret f (Poly m) = Poly $ \k -> m $ \u ->
  case prjCoerce u of
    Left x -> k $ hoist (reinterpret' f) $ x
    Right y  -> usingPoly k $ f y
{-# INLINE[3] reinterpret #-}


reinterpret'
    :: Effect f
    => (∀ x. f (Poly (f ': r)) x -> Poly (g ': r) x)
    -> Poly (f ': r) a
    -> Poly (g ': r) a
reinterpret' = reinterpret
{-# NOINLINE reinterpret' #-}


runRelayS
    :: Effect e
    => (∀ x. e (Poly (e ': r)) x -> s -> Poly r (s, x))
    -> s
    -> Poly (e ': r) a
    -> Poly r (s, a)
runRelayS f = stateful $ \e -> S.StateT $ fmap swap . f e
{-# INLINE runRelayS #-}

