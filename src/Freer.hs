{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}
{-# OPTIONS_GHC -Wall              #-}

module Freer where

import Control.Monad




class Syntax r where
  emap
      :: (m a -> m b)
      -> r m a
      -> r m b
  weave
      :: (Monad m, Monad n, Functor s)
      => s ()
      -> (forall x. s (m x) -> n (s x))
      -> r m a
      -> r n (s a)


data Exc e m a where
  Throw :: e -> Exc e m a
  Catch :: m x
        -> (e -> m x)
        -> (x -> m a)
        -> Exc e m a


instance Functor m => Functor (Exc e m) where
  fmap _ (Throw e) = Throw e
  fmap f (Catch a b c) = Catch a b (fmap f . c)



instance Syntax (Exc e) where
  emap _ (Throw e) = Throw e
  emap f (Catch a b c) = Catch a b $ fmap f c

  weave _ _ (Throw e) = Throw e
  weave s distrib (Catch a b c) =
    Catch
      (distrib (a <$ s))
      (\e -> distrib $ b e <$ s)
      (distrib . fmap c)



data Free r a
  = Pure a
  | Free (r (Free r) a)

data (:+:) (f :: (* -> *) -> * -> *) g m a
  = L (f m a)
  | R (g m a)
  deriving Functor

instance Syntax r => Functor (Free r) where
  fmap f (Pure a) = Pure $ f a
  fmap f (Free z) = Free $ emap (fmap f) z

instance Syntax r => Applicative (Free r) where
  pure = Pure
  (<*>) = ap

instance Syntax r => Monad (Free r) where
  return = Pure
  Pure v >>= prog = prog v
  Free op >>= prog = Free (emap (>>= prog) op)


instance (Syntax f, Syntax g) => Syntax (f :+: g) where
  emap f (L l) = L $ emap f l
  emap f (R r) = R $ emap f r

  weave s hdl (L l) = L $ weave s hdl l
  weave s hdl (R r) = R $ weave s hdl r

class (Syntax sub, Syntax sup) => Member sub sup where
  inj :: sub m a -> sup m a
  prj :: sup m a -> Maybe (sub m a)

instance {-# OVERLAPPABLE #-} Syntax r => Member r r where
  inj = id
  prj = Just

instance {-# OVERLAPPING #-} (Syntax f, Syntax g) => Member f (f :+: g) where
  inj = L
  prj (L f) = Just f
  prj _ = Nothing

instance (Syntax r1, Member r r2) => Member r (r1 :+: r2) where
  inj = R . inj
  prj (R g) = prj g
  prj _ = Nothing

inject :: (Member f r) => f (Free r) a -> Free r a
inject = Free . inj

project :: (Member f r) => Free r a -> Maybe (f (Free r) a)
project (Free s) = prj s
project _ = Nothing

type (~>) f g = forall x. f x -> g x

data State s a where
  Get :: (s -> a) -> State s a
  Put :: s -> a -> State s a
  deriving Functor

newtype Lift r (m :: * -> *) a = Lift (r (m a))
  deriving Functor

instance Functor r => Syntax (Lift r) where
  emap f (Lift op) = Lift $ fmap f op
  weave s distrib (Lift op) = Lift $ fmap (distrib . (<$ s)) op

runState
    :: Syntax sig
    => s
    -> Free (Lift (State s) :+: sig) a
    -> Free sig (s, a)
runState s (Pure a) = Pure (s, a)
runState s (Free (L (Lift (Get k)))) = runState s $ k s
runState _ (Free (L (Lift (Put s k)))) = runState s k
runState s (Free (R k)) = Free $ weave (s, ()) (uncurry runState) k


runExc
    :: Syntax sig
    => Free (Exc e :+: sig) a
    -> Free sig (Either e a)
runExc (Pure a) = Pure $ Right a
runExc (Free (L (Throw e))) = Pure $ Left e
runExc (Free (L (Catch a b c))) = do
  r <- runExc a
  case r of
    Right z -> runExc $ c z
    Left e -> do
      z <- runExc $ b e
      case z of
        Left e' -> pure $ Left e'
        Right x -> runExc $ c x
runExc (Free (R k)) = Free $
  weave (Right ()) (either (pure . Left) runExc) k

data Void a deriving Functor


runM :: Monad m => Free (Lift m) a -> m a
runM (Pure a) = pure a
runM (Free (Lift m)) = m >>= runM


run :: Free (Lift Void) a -> a
run (Pure a) = a
run (Free (Lift _)) = error "impossible"


send :: Member e r => (forall k. (a -> k) -> e (Free r) k) -> Free r a
send m = Free . inj $ m id

sendIt :: Member e r => e (Free r) a -> Free r a
sendIt = Free . inj

sendL
    :: (Member (Lift e) r, Functor e)
    => (forall k. (a -> k) -> e k)
    -> Free r a
sendL m = sendM $ m id

sendItL
    :: (Member (Lift e) r, Functor e)
    => (Free r () -> e (Free r ()))
    -> Free r ()
sendItL m = Free $ inj $ Lift $ m $ pure ()

sendM :: Functor m => Member (Lift m) r => m a -> Free r a
sendM = Free . inj . Lift . fmap Pure


main :: IO ()
main = (print =<<) $ runM $ runState "hi" $ runExc @_ @Int $ do
  z <- sendIt @(Exc Int) $
    Catch
      (do
        s <- sendL Get
        sendM $ putStrLn s
        sendItL $ Put "error!"
        void $ sendIt $ Throw (5 :: Int)
        pure False)
      (\_ -> do
        s <- sendL Get
        sendM $ putStrLn s
        sendItL $ Put "everything is ok now"
        pure True)
      pure

  sendM $ putStrLn $ "caught it: " ++ show z

  s <- sendL Get
  sendM $ putStrLn s




