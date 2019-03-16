{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE BlockArguments        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveFunctor         #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DerivingVia           #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE UnicodeSyntax         #-}

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all             #-}

module TRYAGAIN where

import Control.Monad
import Data.Functor.Identity
import Unsafe.Coerce
import Data.Coerce


newtype Lift m (z :: * -> *) a = Lift
  { unLift :: m a
  }
  deriving (Functor, Applicative, Monad) via m


instance Functor m => Effect (Lift m) where
  weave s _ (Lift a) = Lift $ fmap (<$ s) a
  {-# INLINE weave #-}

  hoist _ a = coerce a
  {-# INLINE hoist #-}


newtype F f a = F
  { runF
        :: ∀ r
         . (a -> r)
        -> (f (F f) r -> r)
        -> r
  -- TODO(sandy): If I can final encode this, we'll get hot speed gainz.
  }

runEff :: (a -> r) -> (f (F f) r -> r) -> F f a -> r
runEff kp kf e = runF e kp kf


data Union (r :: [(* -> *) -> * -> *]) (m :: * -> *) a where
  Union :: Effect e => Word -> e m a -> Union r m a


unsafeInj :: Effect e => Word -> e m a -> Union r m a
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
  {-# INLINE elemNo #-}


instance {-# OVERLAPPABLE #-} FindElem t r => FindElem t (t' ': r) where
  elemNo = P $ 1 + unP (elemNo :: P t r)
  {-# INLINE elemNo #-}


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


weaken :: Union r m a -> Union (e ': r) m a
weaken (Union n a) = Union (n + 1) a


raise :: Eff r a -> Eff (e ': r) a
raise = runEff pure $ join . liftEff . hoist raise . weaken


instance Functor m => Functor (Union r m) where
  fmap f (Union w t) = Union w $ fmap f t
  {-# INLINE fmap #-}


class (∀ m. Functor m => Functor (e m)) => Effect e where
  weave
      :: (Functor s, Functor m)
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)

  default weave
      :: ( Coercible (e m (s a)) (e n (s a))
         , Functor s
         , Functor m
         )
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)
  weave s _ = coerce . fmap (<$ s)
  {-# INLINE weave #-}

  hoist :: (Functor m, Functor n) => (∀ x. m x -> n x) -> e m a -> e n a
  hoist f = fmap runIdentity
          . weave (Identity ())
                  (fmap Identity . f . runIdentity)
  {-# INLINE hoist #-}



instance Effect (Union r) where
  weave s f (Union w e) = Union w $ weave s f e
  {-# INLINE weave #-}


type Eff r = F (Union r)


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)


get :: Member (State s) r => Eff r s
get = send $ Get id


put :: Member (State s) r => s -> Eff r ()
put s = send $ Put s ()


data Error e m a
  = Throw e
  | ∀ x. Catch (m x) (e -> m x) (x -> a)

deriving instance Functor (Error e m)

instance Effect (Error e) where
  weave _ _ (Throw e) = Throw e
  weave s f (Catch try handle k) =
    Catch (f $ try <$ s) (\e -> f $ handle e <$ s) $ fmap k
  {-# INLINE weave #-}


throw :: Member (Error e) r => e -> Eff r a
throw = send . Throw


catch :: Member (Error e) r => Eff r a -> (e -> Eff r a) -> Eff r a
catch try handle = send $ Catch try handle id


runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState = runRelayS (\s x -> pure (s, x)) $ \case
  Get k   -> \s -> k s s
  Put s k -> const $ k s
{-# INLINE runState #-}


interpret
    :: (∀ x. e (Eff (e ': r)) (Eff r x) -> Eff r x)
    -> Eff (e ': r) a
    -> Eff r a
interpret f = runEff pure $ \u ->
  case decomp u of
    Left x -> join . liftEff $ hoist (interpret f) x
    Right eff -> f eff


interpose
    :: Member e r
    => (∀ x. e (Eff r) x -> Eff r x)
    -> Eff r a
    -> Eff r a
interpose f = runEff pure $ \u ->
  join $ case prj u of
    Just x  -> f x
    Nothing -> liftEff u


subsume
    :: (Member e r, Effect e)
    => Eff (e ': r) a
    -> Eff r a
subsume = interpret $ join . send . hoist subsume


reinterpret
    :: Effect f
    => (∀ x. f (Eff (g ': r)) x -> Eff (g ': r) x)
    -> Eff (f ': r) a
    -> Eff (g ': r) a
reinterpret f = runEff pure $ \u ->
  join $ case decomp u of
    Left  x -> liftEff $ weaken $ hoist (reinterpret f) x
    Right y -> f $ hoist (reinterpret f) $ y


-- TODO(sandy): does this have the right semantics for INSIDE MONADS?
translate
    :: ( Effect f
       , Effect g
       )
    => (∀ x. f (Eff (f ': r)) x -> g (Eff (g ': r)) x)
    -> Eff (f ': r) a
    -> Eff (g ': r) a
translate f = runEff pure $ \u ->
  join $ case decomp u of
    Left  x -> liftEff $ weaken $ hoist (translate f) x
    Right y -> send $ f y


runRelayS
    :: ∀ s e a r
     . (∀ x. s -> x -> Eff r (s, x))
    -> (∀ x. e (Eff (e ': r)) (s -> Eff r (s, x))
          -> s
          -> Eff r (s, x))
    -> s
    -> Eff (e ': r) a
    -> Eff r (s, a)
runRelayS pure' bind' = flip go
  where
    go :: Eff (e ': r) x -> s -> Eff r (s, x)
    go = runEff (flip pure') $ \u ->
      case decomp u of
        Left x  -> \s' ->
          join . liftEff
               $ fmap (uncurry (flip id))
               $ weave (s', ()) (uncurry $ flip go) x
        Right eff -> bind' eff


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError = runEff (pure . Right) $ \u ->
  case decomp u of
    Left x  -> join
             . liftEff
             . fmap (either (pure . Left) id)
             $ weave (Right ()) (either (pure . Left) runError) x
    Right (Throw e) -> pure $ Left e
    Right (Catch try handle k) -> do
      ma <- runError try
      case ma of
        Right a -> k a
        Left e -> do
          ma' <- runError $ handle e
          case ma' of
            Left e' -> pure (Left e')
            Right a -> k a
{-# INLINE runError #-}


runM :: Monad m => Eff '[Lift m] a -> m a
runM e = runF e pure $ join . unLift . extract
{-# INLINE runM #-}


run :: Eff '[Lift Identity] a -> a
run = runIdentity . runM
{-# INLINE run #-}


send :: Member eff r => eff (Eff r) a -> Eff r a
send = liftEff . inj
{-# INLINE send #-}


sendM :: Member (Lift m) r => m a -> Eff r a
sendM = liftEff . inj . Lift
{-# INLINE sendM #-}


main :: IO ()
main = (print =<<) $ runM $ runState "1" $ runError @Bool $ runState True $ do
  put "2"
  catch
    do
      sendM $ putStrLn "hello"
      put False
      throw True
    \(_ :: Bool) -> do
      sendM $ putStrLn "caught"


extract :: Union '[e] m a -> e m a
extract (Union _ a) = unsafeCoerce a
{-# INLINE extract #-}


liftEff :: Union r (Eff r) a -> Eff r a
liftEff u = F $ \kp kf -> kf $ fmap kp u
{-# INLINE liftEff #-}


instance Functor (F f) where
  fmap f (F g) = F (\kp -> g (kp . f))
  {-# INLINE fmap #-}


instance Applicative (F f) where
  pure a = F (\kp _ -> kp a)
  {-# INLINE pure #-}
  F f <*> F g = F (\kp kf -> f (\a -> g (kp . a) kf) kf)
  {-# INLINE (<*>) #-}


instance Monad (F f) where
  return = pure
  {-# INLINE return #-}
  F m >>= f = F (\kp kf -> m (\a -> runF (f a) kp kf) kf)
  {-# INLINE (>>=) #-}

