{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

module TRYAGAIN where

import Control.Monad.Trans.Class
import Control.Monad
import Unsafe.Coerce


newtype Lift m (z :: * -> *) a = Lift
  { unLift :: m a
  }
  deriving (Functor, Applicative, Monad) via m

instance Monad m => Effect (Lift m) where
  weave s _ (Lift a) = Lift $ fmap (<$ s) a

newtype F f a = F
  { runF
        :: forall r
         . (a -> r)
        -> (f (F f) r -> r)
        -> r
  }

data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union :: Effect e => Word -> e m a -> Union r m a

unsafeInj :: (Monad m, Effect e) => Word -> e m a -> Union r m a
unsafeInj w = Union w
{-# INLINE unsafeInj #-}

unsafePrj :: Word -> Union r m a -> Maybe (t m a)
unsafePrj n (Union n' x)
  | n == n'   = Just (unsafeCoerce x)
  | otherwise = Nothing
{-# INLINE unsafePrj #-}

newtype P t r = P {unP :: Word}

class FindElem (t :: k) (r :: [k]) where
  elemNo :: P t r

instance FindElem t (t ': r) where
  elemNo = P 0

instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)

class FindElem eff effs
      => Member (eff :: (* -> *) -> * -> *)
                (effs :: [(* -> *) -> * -> *]) where
  inj :: Monad m => eff m a -> Union effs m a
  prj :: Union effs m a -> Maybe (eff m a)

instance (Effect t, FindElem t r) => Member t r where
  inj = unsafeInj $ unP (elemNo :: P t r)
  {-# INLINE inj #-}

  prj = unsafePrj $ unP (elemNo :: P t r)
  {-# INLINE prj #-}

decomp :: Union (t ': r) m a -> Either (Union r m a) (e m a)
decomp (Union 0 a) = Right $ unsafeCoerce a
decomp (Union n a) = Left  $ Union (n - 1) a
{-# INLINE [2] decomp #-}


instance Monad m => Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap f t



class (forall m. Monad m => Functor (e m)) => Effect e where
  weave
      :: (Monad m, Monad n, Functor s)
      => s ()
      -> (forall x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)

instance Effect (Union r) where
  weave s f (Union w e) = Union w $ weave s f e



type Eff r = F (Union r)


data State s (m :: * -> *) a
  = Get (s -> a)
  | Put s a
  deriving Functor

instance Effect (State s) where
  weave s _ (Get k) = Get $ fmap (<$ s) k
  weave s _ (Put z k) = Put z $ k <$ s


runState :: Eff (State s ': r) a -> s -> Eff r (s, a)
runState e = runF e (\a s -> pure (s, a)) $ \u ->
  case decomp u of
    Left x  -> \s' -> zoop $ fmap (uncurry (flip id)) $ weave (s', ()) (uncurry $ flip runState) x
    Right (Get k) -> \s' -> k s' s'
    Right (Put s' k) -> const $ k s'


runM :: Monad m => Eff '[Lift m] a -> m a
runM e = runF e pure $ join . unLift . extract

send :: Member eff r => eff (Eff r) a -> Eff r a
send = liftEff . inj

sendM :: Member (Lift m) r => m a -> Eff r a
sendM = liftEff . inj . Lift


main :: IO ()
main = (print =<<) $ runM $ flip runState True $ flip runState "1" $ do
  send $ Put "2" ()
  sendM $ putStrLn "hello"
  send $ Put False ()


extract :: Union '[e] m a -> e m a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}



liftEff :: Union r (Eff r) a -> Eff r a
liftEff u = F $ \kp kf -> kf $ fmap kp u


zoop :: Union r (Eff r) (Eff r a) -> Eff r a
zoop m = join $ liftEff m


iter :: (f (F f) a -> a) -> F f a -> a
iter phi xs = runF xs id phi


iterM :: Monad m => (f (F f) (m a) -> m a) -> F f a -> m a
iterM phi xs = runF xs pure phi


instance Functor (F f) where
  fmap f (F g) = F (\kp -> g (kp . f))


instance Applicative (F f) where
  pure a = F (\kp _ -> kp a)
  F f <*> F g = F (\kp kf -> f (\a -> g (kp . a) kf) kf)


instance Monad (F f) where
  return = pure
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)

