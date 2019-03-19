{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all             #-}

module TRYAGAIN where

import           Definitive
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)


get :: Member (State s) r => Def r s
get = send $ Get id
{-# INLINE get #-}


put :: Member (State s) r => s -> Def r ()
put s = send $ Put s ()
{-# INLINE put #-}


data Error e m a
  = Throw e
  | ∀ x. Catch (m x) (e -> m x) (x -> a)

deriving instance Functor (Error e m)

instance Effect (Error e) where
  weave _ _ (Throw e) = Throw e
  weave s f (Catch try handle k) =
    Catch (f $ try <$ s) (\e -> f $ handle e <$ s) $ fmap k
  {-# INLINE weave #-}

  hoist = slowDefaultHoist
  {-# INLINE hoist #-}


throw :: Member (Error e) r => e -> Def r a
throw = send . Throw


catch :: Member (Error e) r => Def r a -> (e -> Def r a) -> Def r a
catch try handle = send $ Catch try handle id



runState :: s -> Def (State s ': r) a -> Def r (s, a)
runState = stateful $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE[3] runState #-}


{-# RULES "runState/reinterpret"
   forall s e (f :: forall x. e (Def (e ': r)) x -> Def (State s ': r) x).
     runState s (reinterpret f e) = runRelayS (\x s' -> runState s' $ f x) s e
     #-}


runError :: Def (Error e ': r) a -> Def r (Either e a)
runError (Freer m) = Freer $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left x -> E.ExceptT $ k $ weave (Right ()) (either (pure . Left) runError') x
    Right (Throw e) -> E.throwE e
    Right (Catch try handle kt) -> E.ExceptT $ do
      let zonk = usingFreer k . runError'
      ma <- zonk try
      case ma of
        Right a -> pure . Right $ kt a
        Left e -> do
          ma' <- zonk $ handle e
          case ma' of
            Left e' -> pure $ Left e'
            Right a -> pure . Right $ kt a
{-# INLINE runError #-}


runError' :: Def (Error e ': r) a -> Def r (Either e a)
runError' = runError
{-# NOINLINE runError' #-}

