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

import           Control.Monad.Discount
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S


data State s m a
  = Get (s -> a)
  | Put s a
  deriving (Functor, Effect)


get :: Member (State s) r => Eff r s
get = send $ Get id
{-# INLINE get #-}


put :: Member (State s) r => s -> Eff r ()
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


throw :: Member (Error e) r => e -> Eff r a
throw = send . Throw


catch :: Member (Error e) r => Eff r a -> (e -> Eff r a) -> Eff r a
catch try handle = send $ Catch try handle id


runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState = stateful $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE runState #-}


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
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


runError' :: Eff (Error e ': r) a -> Eff r (Either e a)
runError' = runError
{-# NOINLINE runError' #-}

