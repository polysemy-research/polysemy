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

import Data.Tuple
import Control.Monad.Discount
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

runStateFast
    :: forall s r a
     . s
    -> Eff (State s ': r) a
    -> Eff r (s, a)
runStateFast sz fm = go sz fm
  where
    go s (Freer m) = Freer $ \k ->
      fmap swap $ flip S.runStateT s $ m $ \u ->
        case decomp u of
            Left x -> S.StateT $ \s' ->
              k . fmap swap
                . weave (s', ()) (uncurry runStateFast')
                $ x
            Right (Get k2) -> fmap k2 S.get
            Right (Put s' k2) -> S.put s' >> pure k2
{-# INLINE runStateFast #-}

runStateFast'
    :: forall s r a
     . s
    -> Eff (State s ': r) a
    -> Eff r (s, a)
runStateFast' s (Freer m) = Freer $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry runStateFast)
            $ x
        Right (Get k2) -> fmap k2 S.get
        Right (Put s' k2) -> S.put s' >> pure k2
{-# INLINE runStateFast' #-}


runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState = stateful $ \case
  Get k   -> fmap k S.get
  Put s k -> S.put s >> pure k
{-# INLINE runState #-}



-- runRelayS
--     :: ∀ s e a r
--      . (∀ x. s -> x -> Eff r (s, x))
--     -> (∀ x. e (Eff (e ': r)) (s -> Eff r (s, x))
--           -> s
--           -> Eff r (s, x))
--     -> s
--     -> Eff (e ': r) a
--     -> Eff r (s, a)
-- runRelayS pure' bind' = flip go
--   where
--     go :: Eff (e ': r) x -> s -> Eff r (s, x)
--     go = runEff (flip pure') $ \u ->
--       case decomp u of
--         Left x  -> \s' ->
--           join . liftEff
--                $ fmap (uncurry (flip id))
--                $ weave (s', ()) (uncurry $ flip go) x
--         Right eff -> bind' eff
-- {-# INLINE runRelayS #-}


-- runError :: Eff (Error e ': r) a -> Eff r (Either e a)
-- runError = runEff (pure . Right) $ \u ->
--   case decomp u of
--     Left x  -> join
--              . liftEff
--              . fmap (either (pure . Left) id)
--              $ weave (Right ()) (either (pure . Left) runError) x
--     Right (Throw e) -> pure $ Left e
--     Right (Catch try handle k) -> do
--       ma <- runError try
--       case ma of
--         Right a -> k a
--         Left e -> do
--           ma' <- runError $ handle e
--           case ma' of
--             Left e' -> pure (Left e')
--             Right a -> k a
-- {-# INLINE runError #-}



-- main :: IO ()
-- main = (print =<<) $ runM $ runState "1" $ runError @Bool $ runState True $ do
--   put "2"
--   catch
--     do
--       sendM $ putStrLn "hello"
--       put False
--       throw True
--     \(_ :: Bool) -> do
--       sendM $ putStrLn "caught"



