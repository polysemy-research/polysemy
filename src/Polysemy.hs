{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Polysemy
  ( Semantic (..)
  , Member
  , send
  , sendM
  , run
  , runM
  , raise
  , Lift ()
  , usingSemantic
  , liftSemantic
  , hoistSemantic
  ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Polysemy.Effect
import Polysemy.Lift
import Polysemy.Fixpoint.Type
import Polysemy.NonDet.Type
import Polysemy.Union


newtype Semantic r a = Semantic
  { runSemantic
        :: ∀ m
         . Monad m
        => (∀ x. Union r (Semantic r) x -> m x)
        -> m a
  }

------------------------------------------------------------------------------
-- | Like 'runSemantic' but flipped for better ergonomics sometimes.
usingSemantic
    :: Monad m
    => (∀ x. Union r (Semantic r) x -> m x)
    -> Semantic r a
    -> m a
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


instance (Member NonDet r) => Alternative (Semantic r) where
  empty = send Empty
  a <|> b = do
    send (Choose id) >>= \case
      False -> a
      True  -> b


instance (Member (Lift IO) r) => MonadIO (Semantic r) where
  liftIO = sendM
  {-# INLINE liftIO #-}

instance Member Fixpoint r => MonadFix (Semantic r) where
  mfix f = send $ Fixpoint f id


liftSemantic :: Union r (Semantic r) a -> Semantic r a
liftSemantic u = Semantic $ \k -> k u
{-# INLINE liftSemantic #-}


hoistSemantic
    :: (∀ x. Union r (Semantic r) x -> Union r' (Semantic r') x)
    -> Semantic r a
    -> Semantic r' a
hoistSemantic nat (Semantic m) = Semantic $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistSemantic #-}


raise :: Semantic r a -> Semantic (e ': r) a
raise = hoistSemantic $ hoist raise' . weaken
{-# INLINE raise #-}


raise' :: Semantic r a -> Semantic (e ': r) a
raise' = raise
{-# NOINLINE raise' #-}


send :: Member e r => e (Semantic r) a -> Semantic r a
send = liftSemantic . inj
{-# INLINE[3] send #-}


------------------------------------------------------------------------------
-- | Lift a monadic action @m@ into 'Semantic'.
sendM :: Member (Lift m) r => m a -> Semantic r a
sendM = send . Lift
{-# INLINE sendM #-}


------------------------------------------------------------------------------
-- | Run a 'Semantic' containing no effects as a pure value.
run :: Semantic '[] a -> a
run (Semantic m) = runIdentity $ m absurdU
{-# INLINE run #-}


------------------------------------------------------------------------------
-- | Lower a 'Semantic' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Semantic '[Lift m] a -> m a
runM (Semantic m) = m $ unLift . extract
{-# INLINE runM #-}

