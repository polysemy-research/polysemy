{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
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


import qualified Control.Exception as X
import           Control.Monad
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State hiding (State (..), runState)
import           Data.Functor.Compose
import           Data.Functor.Coyoneda
import           Data.Functor.Identity
import           Data.Kind
import           Data.OpenUnion.Internal
import           Data.OpenUnion.Internal
import           Data.Tuple
import           Data.Void
import           Eff.Type
import           Unsafe.Coerce


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Error e (m :: * -> *) a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a


data Scoped (m :: * -> *) a where
  Scoped :: m () -> m a -> Scoped m a


foo
    :: ( Member (State String) r
       , Member (Error Bool) r
       , Member (Lift IO) r
       )
    => Eff r ()
foo = do
  z <- send $ Catch
    do
      send Get >>= sendM . putStrLn
      send $ Put "works"
      send Get >>= sendM . putStrLn
      send $ Put "done"
      send $ Throw False
      sendM $ putStrLn "inside"
      pure False
    \False -> do
      send Get >>= sendM . putStrLn
      send $ Put "caught"
      pure True
  send Get >>= sendM . putStrLn
  sendM $ print z


main :: IO ()
main = (print =<<) . runM . runState "first" . runError @Bool $ foo

weave :: Monad m => (forall x. t m x -> m (f x)) -> Union r (t m) a -> Union r m (f a)
weave distrib (Union w (Yo e nt)) = Union w $ Yo e $ distrib . nt

reassoc
    :: Monad m
    => (forall x. t m x -> m (f x))
    -> (forall x n. n (f x) -> t n x)
    -> Union r (t m) a
    -> t (Union r m) a
reassoc distrib assoc = assoc . weave distrib

-- distribute :: (forall x. Union r (t m) x -> t (Union r m) x)


transform
    :: forall e t m r a f
     . (forall m. Monad m => Monad (t m))
    => (forall x m. t m x -> m (f x))
    -> (forall x m. m (f x) -> t m x)
    -> (forall m. e m ~> t (Eff r))
    -> Eff (e ': r) a
    -> Eff r (f a)
transform lower raise f  = lower . go
  where
    go :: forall x. Eff (e ': r) x -> t (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left  x -> raise . liftEff . weave lower $ hoist go x
        Right (Yo z nt) -> do
          a <- f z
          go $ nt $ pure a



runState' :: Eff (State s ': r) a -> StateT s (Eff r) a
runState' (Freer m) = m $ \u ->
  case decomp u of
    Left x -> do
      StateT $ \s -> fmap swap . liftEff . weave (fmap swap . flip runStateT s) $ hoist runState' x
    Right (Yo Get f) -> do
      z <- get
      runState' $ f $ pure z
    Right (Yo (Put s) f) -> do
      put s
      runState' $ f $ pure ()


runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState s = fmap swap . flip runStateT s . runState'


runError' :: Eff (Error e ': r) a -> E.ExceptT e (Eff r) a
runError' (Freer m) = m $ \u ->
  case decomp u of
    Left x -> do
      E.ExceptT . liftEff . weave E.runExceptT $ hoist runError' x
    Right (Yo (Throw e) _) -> E.throwE e
    Right (Yo (Catch try catch) f) -> E.ExceptT $ do
      err <- E.runExceptT $ runError' $ f try
      case err of
        Right a -> pure (Right a)
        Left e -> do
          err' <- E.runExceptT $ runError' $ f $ catch e
          case err' of
            Left e' -> pure (Left e')
            Right a -> pure (Right a)

runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError = E.runExceptT . runError'

