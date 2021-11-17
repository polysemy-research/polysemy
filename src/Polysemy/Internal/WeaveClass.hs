{-# LANGUAGE GeneralizedNewtypeDeriving, QuantifiedConstraints, TupleSections #-}
{-# OPTIONS_HADDOCK not-home #-}
module Polysemy.Internal.WeaveClass
  ( MonadTransWeave(..)

  , mkInitState
  , mkDistrib
  , Distrib(..)
  , mkInspector

  , ComposeT(..)
  ) where

import Control.Monad
import Control.Monad.Trans
import qualified Control.Monad.Trans.Except as E
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import qualified Control.Monad.Trans.State.Lazy as LSt
import qualified Control.Monad.Trans.State.Strict as SSt
import qualified Control.Monad.Trans.Writer.Lazy as LWr
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Tuple

-- | A variant of the classic @MonadTransControl@ class from @monad-control@,
-- but with a small number of changes to make it more suitable for Polysemy's
-- internals.
class ( MonadTrans t
      , forall z. Monad z => Monad (t z)
      , Traversable (StT t)
      )
   => MonadTransWeave t where
  type StT t :: * -> *

  -- inspect :: Foldable f => f a -> Maybe a
  -- inspect = foldr (const . Just) Nothing

  hoistT :: (Monad m, Monad n)
         => (forall x. m x -> n x)
         -> t m a -> t n a
  hoistT n m = controlT $ \lower -> n (lower m)
  {-# INLINE hoistT #-}

  controlT :: Monad m
           => ((forall z x. Monad z => t z x -> z (StT t x)) -> m (StT t a))
           -> t m a
  controlT main = liftWith main >>= restoreT . pure
  {-# INLINE controlT #-}

  liftWith :: Monad m
           => ((forall z x. Monad z => t z x -> z (StT t x)) -> m a)
           -> t m a

  restoreT :: Monad m => m (StT t a) -> t m a

newtype ComposeT t (u :: (* -> *) -> * -> *) m a = ComposeT {
    getComposeT :: t (u m) a
  }
  deriving (Functor, Applicative, Monad)

instance ( MonadTrans t
         , MonadTrans u
         , forall m. Monad m => Monad (u m)
         )
      => MonadTrans (ComposeT t u) where
  lift m = ComposeT (lift (lift m))

instance ( MonadTransWeave t
         , MonadTransWeave u
         )
      => MonadTransWeave (ComposeT t u) where
  type StT (ComposeT t u) = Compose (StT u) (StT t)

  hoistT n (ComposeT m) = ComposeT (hoistT (hoistT n) m)

  controlT main = ComposeT $
    controlT $ \lowerT ->
    controlT $ \lowerU ->
    getCompose <$> main (\(ComposeT m) -> Compose <$> lowerU (lowerT m))

  liftWith main = ComposeT $
    liftWith $ \lowerT ->
    liftWith $ \lowerU ->
    main (\(ComposeT m) -> Compose <$> lowerU (lowerT m))

  restoreT m = ComposeT (restoreT (restoreT (fmap getCompose m)))

newtype Distrib f q m = Distrib (forall x. f (q x) -> m (f x))

mkInitState :: Monad (t Identity)
            => (t Identity () -> Identity (StT t ()))
            -> StT t ()
mkInitState lwr = runIdentity $ lwr (pure ())
{-# INLINE mkInitState #-}

mkDistrib :: (MonadTransWeave t, Monad m)
          => (forall n x. Monad n => (forall y. m y -> n y) -> q x -> t n x)
          -> (forall z x. Monad z => t z x -> z (StT t x))
          -> Distrib (StT t) q m
mkDistrib mkT lwr = Distrib $ lwr . join . restoreT . return . fmap (mkT id)
{-# INLINE mkDistrib #-}

mkInspector :: Foldable f => f a -> Maybe a
mkInspector = foldr (const . Just) Nothing
{-# INLINE mkInspector #-}

instance MonadTransWeave IdentityT where
  type StT IdentityT = Identity
  hoistT nt = IdentityT . nt . runIdentityT

  liftWith main = IdentityT (main (fmap Identity . runIdentityT))

  controlT main = IdentityT (runIdentity <$> main (fmap Identity . runIdentityT))

  restoreT = IdentityT . fmap runIdentity

instance MonadTransWeave (LSt.StateT s) where
  type StT (LSt.StateT s) = (,) s

  hoistT nt = LSt.mapStateT nt

  controlT main = LSt.StateT $ \s ->
    swap <$> main (\m -> swap <$> LSt.runStateT m s)

  liftWith main = LSt.StateT $ \s ->
        (, s)
    <$> main (\m -> swap <$> LSt.runStateT m s)

  restoreT m = LSt.StateT $ \_ -> swap <$> m

instance MonadTransWeave (SSt.StateT s) where
  type StT (SSt.StateT s) = (,) s

  hoistT nt = SSt.mapStateT nt

  controlT main = SSt.StateT $ \s ->
    swap <$!> main (\m -> swap <$!> SSt.runStateT m s)

  liftWith main = SSt.StateT $ \s ->
        (, s)
    <$> main (\m -> swap <$!> SSt.runStateT m s)

  restoreT m = SSt.StateT $ \_ -> swap <$!> m

instance MonadTransWeave (E.ExceptT e) where
  type StT (E.ExceptT e) = Either e

  hoistT nt = E.mapExceptT nt

  controlT main = E.ExceptT (main E.runExceptT)

  liftWith main = lift $ main E.runExceptT

  restoreT = E.ExceptT

instance Monoid w => MonadTransWeave (LWr.WriterT w) where
  type StT (LWr.WriterT w) = (,) w

  hoistT nt = LWr.mapWriterT nt

  controlT main = LWr.WriterT (swap <$> main (fmap swap . LWr.runWriterT))

  liftWith main = lift $ main (fmap swap . LWr.runWriterT)

  restoreT m = LWr.WriterT (swap <$> m)


instance MonadTransWeave MaybeT where
  type StT MaybeT = Maybe

  hoistT nt = mapMaybeT nt

  controlT main = MaybeT (main runMaybeT)

  liftWith main = lift $ main runMaybeT

  restoreT = MaybeT
