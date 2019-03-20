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


newtype Semantic r a = Semantic
  { runSemantic
        :: ∀ m
         . Monad m
        => (∀ x. Union r (Semantic r) x -> m x)
        -> m a
  }

usingSemantic :: Monad m => (∀ x. Union r (Semantic r) x -> m x) -> Semantic r a -> m a
usingSemantic k m = runSemantic m k
{-# INLINE usingSemantic #-}


instance Functor (Semantic f) where
  fmap f (Semantic m) = Semantic $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Semantic f) where
  pure a = Semantic $ const $ pure a
  {-# INLINE pure #-}

  Semantic f <*> Semantic a = Semantic $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Semantic f) where
  return = pure
  {-# INLINE return #-}

  Semantic ma >>= f = Semantic $ \k -> do
    z <- ma k
    runSemantic (f z) k
  {-# INLINE (>>=) #-}


instance (Member (Lift IO) r) => MonadIO (Semantic r) where
  liftIO = sendM
  {-# INLINE liftIO #-}

instance MonadFix (Semantic '[]) where
  mfix f = a
    where
      a = f (run a)


liftSemantic :: Union r (Semantic r) a -> Semantic r a
liftSemantic u = Semantic $ \k -> k u
{-# INLINE liftSemantic #-}


hoistSemantic
    :: (∀ x. Union r (Semantic r) x -> Union r' (Semantic r') x)
    -> Semantic r a
    -> Semantic r' a
hoistSemantic nat (Semantic m) = Semantic $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistSemantic #-}


send :: Member e r => e (Semantic r) a -> Semantic r a
send = liftSemantic . inj
{-# INLINE[3] send #-}


sendM :: Member (Lift m) r => m a -> Semantic r a
sendM = send . Lift
{-# INLINE sendM #-}


run :: Semantic '[] a -> a
run (Semantic m) = runIdentity $ m absurdU
{-# INLINE run #-}


runM :: Monad m => Semantic '[Lift m] a -> m a
runM (Semantic m) = m $ unLift . extract
{-# INLINE runM #-}


interpret
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpret f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpret f) x
    Right y -> f y
{-# INLINE interpret #-}


stateful
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
stateful f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ stateful' f)
            $ x
        Right y -> S.mapStateT (usingSemantic k) $ f y
{-# INLINE stateful #-}


stateful'
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
stateful' = stateful
{-# NOINLINE stateful' #-}


reinterpret
    :: Effect f
    => (∀ x. f (Semantic (f ': r)) x -> Semantic (g ': r) x)
    -> Semantic (f ': r) a
    -> Semantic (g ': r) a
reinterpret f (Semantic m) = Semantic $ \k -> m $ \u ->
  case prjCoerce u of
    Left x -> k $ hoist (reinterpret' f) $ x
    Right y  -> usingSemantic k $ f y
{-# INLINE[3] reinterpret #-}


reinterpret'
    :: Effect f
    => (∀ x. f (Semantic (f ': r)) x -> Semantic (g ': r) x)
    -> Semantic (f ': r) a
    -> Semantic (g ': r) a
reinterpret' = reinterpret
{-# NOINLINE reinterpret' #-}


runRelayS
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
runRelayS f = stateful $ \e -> S.StateT $ fmap swap . f e
{-# INLINE runRelayS #-}

