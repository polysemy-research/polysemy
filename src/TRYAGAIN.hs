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
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

{-# OPTIONS_GHC -Wall #-}
-- {-# OPTIONS_GHC -ddump-simpl -dsuppress-all             #-}

module TRYAGAIN where

import Control.Monad.Discount
import Control.Monad


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



