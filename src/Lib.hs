{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UnicodeSyntax         #-}
{-# OPTIONS_GHC -Wall              #-}

module Lib where

import qualified Control.Exception as X
import qualified Control.Monad.Trans.Except as E
import           Data.OpenUnion.Internal
import           Eff.Type
import           StateT


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Error e (m :: * -> *) a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a


data Bracket (m :: * -> *) a where
  Bracket
      :: m a
      -> (a -> m ())
      -> (a -> m r)
      -> Bracket m r


runBracket
    :: Member (Lift IO) r
    => (Eff r ~> IO)
    -> Eff (Bracket ': r) a
    -> Eff r a
runBracket finish = deep $ \start continue -> \case
  Bracket alloc dealloc use -> sendM $
    X.bracket
      (finish $ start alloc)
      (finish . continue dealloc)
      (finish . continue use)
{-# INLINE runBracket #-}


deep
    :: (∀ m tk
           . Functor tk
          => (m ~> Eff r .: tk)
          -> (∀ a b. (a -> m b) -> tk a -> Eff r (tk b))
          -> e m
          ~> Eff r .: tk
       )
    -> Eff (e ': r)
    ~> Eff r
deep f (Freer m) = m $ \u ->
  case decomp u of
    Left  x -> liftEff $ hoist (deep f) x
    Right (Yo e tk nt z) -> fmap z $
      f (deep f . nt . (<$ tk))
        (\ff -> deep f . nt . fmap ff)
        e
{-# INLINE deep #-}


interpretLift
    :: (e ~> Eff r)
    -> Eff (Lift e ': r)
    ~> Eff r
interpretLift f = interpretSimple $ f . unLift
{-# INLINE interpretLift #-}


interpretSimple
    :: (∀ m. e m ~> Eff r)
    -> Eff (e ': r)
    ~> Eff r
interpretSimple f (Freer m) = m $ \u ->
  case decomp u of
    Left  x -> liftEff $ hoist (interpretSimple f) x
    Right (Yo e tk _ z) ->
      fmap (z . (<$ tk)) $ f e
{-# INLINE interpretSimple #-}


statefully
    :: (∀ m. e m ~> StateT s (Eff r))
    -> s
    -> Eff (e ': r) a
    -> Eff r (s, a)
statefully f s =
  shundle
    (StateT . const)
    (flip runStateT s)
    (uncurry $ flip runStateT)
    (s, ()) $ \_ tk -> fmap (<$ tk) . f
{-# INLINE statefully #-}


shundle
   :: ∀ a f t e r
    . ( MonadTrans t
      , ∀ m. Monad m => Monad (t m)
      , Functor f
      )
   => (∀ x. Eff r (f x) -> t (Eff r) x)
   -> (∀ x. t (Eff r) x -> Eff r (f x))
   -> (∀ x. f (t (Eff r) x) -> Eff r (f x))
   -> f ()
   -> (∀ m tk y
          . Functor tk
         => (∀ x. f () -> tk (m x) -> Eff r (f (tk x)))
         -> tk ()
         -> e m y
         -> t (Eff r) (tk y)
      )
   -> Eff (e ': r) a
   -> Eff r (f a)
shundle intro finish dist tk zonk = finish . go
  where
    go :: ∀ x. Eff (e ': r) x -> t (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left x -> intro . liftEff . weave tk dist $ hoist go x
        Right (Yo e sf nt f) -> fmap f $
          zonk (\r -> shundle intro finish dist r zonk . nt) sf e
{-# INLINE shundle #-}


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError =
  shundle
    E.ExceptT
    E.runExceptT
    (either (pure . Left) E.runExceptT)
    (Right ()) $ \start tk -> \case
        Throw e -> E.throwE e
        Catch try handle -> do
          ma <- lift $ start (Right ()) $ (try <$ tk)
          case ma of
            Right a -> pure a
            Left e -> E.ExceptT $ start (Right ()) $ (handle e <$ tk)
{-# INLINE runError #-}


-- runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
-- runState = statefully $ \case
--   Get    -> get
--   Put s' -> put s'
{-# INLINE runState #-}


runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r (s, a)
runState s = flip runStateT s . go
  where
    go :: forall x. Eff (State s ': r) x -> StateT s (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left x -> StateT $ \s' ->
          liftEff . weave (s', ())
                          (uncurry (flip runStateT))
                  $ hoist go x
        Right (Yo Get sf nt f) -> fmap f $ do
          s' <- get
          go $ nt $ pure s' <$ sf
        Right (Yo (Put s') sf nt f) -> fmap f $ do
          put s'
          go $ nt $ pure () <$ sf
