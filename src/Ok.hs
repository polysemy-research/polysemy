{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE QuantifiedConstraints      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}

module Ok where

import Data.Kind
import Data.Functor.Coyoneda
import Unsafe.Coerce
import Data.Void
import Data.OpenUnion.Internal
import Control.Monad.Trans.State hiding (State (..), runState)
import Control.Monad.Trans

newtype Lift m (n :: * -> *) a = Lift
  { unLift :: m a
  }

newtype Eff r a = Eff
  { runEff :: forall m. Monad m => (Union r (Eff r) ~> m) -> m a
  }

usingEff :: Monad m => (Union r (Eff r) ~> m) -> Eff r ~> m
usingEff k e = runEff e k

instance Functor (Eff f) where
  fmap f (Eff m) = Eff $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Eff f) where
  pure a = Eff $ const $ pure a
  {-# INLINE pure #-}

  Eff f <*> Eff a = Eff $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Eff f) where
  return = pure
  {-# INLINE return #-}

  Eff ma >>= f = Eff $ \k -> do
    z <- ma k
    runEff (f z) k
  {-# INLINE (>>=) #-}





voidIt :: Union '[] m a -> Void
voidIt = error "it's a void you dip"


-- -- extract :: Functor m => Union '[Lift m] n a -> m a
-- -- extract u = case decomp u of
-- --   Right (FreeYo (Lift e) f nt) -> fmap f e
-- --   Left u -> absurd $ voidIt u


-- -- decomp
-- --     :: Union (e ': r) m a
-- --     -> Either (Union r m a)
-- --               (FreeYo e m a)
-- -- decomp (Union 0 (FreeYo e m a)) = Right $ FreeYo (unsafeCoerce e) m a
-- -- decomp (Union n e) = Left $ Union (n - 1) e


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Scoped (m :: * -> *) a where
  Scoped :: m () -> m a -> Scoped m a


foo :: (Member (State String) r, Member Scoped r, Member (Lift IO) r) => Eff r ()
foo = send $ Scoped (send Get >>= sendM . putStrLn) $ do
  send Get >>= sendM . putStrLn
  send $ Put "works"
  send Get >>= sendM . putStrLn
  send $ Put "done"
  sendM $ putStrLn "inside"


sendM :: Member (Lift m) r => m a -> Eff r a
sendM m = send $ Lift m


runState :: Member (Lift IO) r => s -> Eff (State s ': r) a -> Eff r a
runState s (Eff m) = Eff $ \k -> flip evalStateT s $ m $ \u ->
  case decomp u of
    Left  x -> do
      s' <- get
      lift $ usingEff k $ liftEff $ hoist (runState s') x
    Right (FreeYo Get _ f) -> fmap f $ get
    Right (FreeYo (Put s') _ f) -> fmap f $ put s'


runScoped :: Eff (Scoped ': r) a -> Eff r a
runScoped (Eff m) = Eff $ \k -> m $ \u ->
  case decomp u of
    Left x -> k $ hoist runScoped x
    Right (FreeYo (Scoped final first) nt f) -> do
      fmap f $ usingEff k $ runScoped $ nt $ first <* final


runM :: Monad m => Eff '[Lift m] a -> m a
runM (Eff m) = m $ \(extract -> FreeYo (Lift e) _ f) -> fmap f e


main :: IO ()
main = runM . runScoped . runState "first" $ foo


liftEff :: Union r (Eff r) a -> Eff r a
liftEff u = Eff $ \k -> k u


send :: Member e r => e (Eff r) a -> Eff r a
send e = Eff $ \k -> k $ inj e


weaken :: Union r m a -> Union (e ': r) m a
weaken (Union w e) = Union (w + 1) e

