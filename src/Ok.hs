{-# LANGUAGE BlockArguments             #-}
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



runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState s (Freer m) = Freer $ \k -> fmap swap $ flip runStateT s $ m $ \u ->
  case decomp u of
    Left  x -> StateT $ \s' ->
      fmap swap $ usingFreer k $ liftEff $ weave (s', ()) (uncurry runState) x
    Right (Yo Get f) ->
      StateT $ \s' -> usingFreer k $ fmap swap $ runState s' $ f $ pure s'
    Right (Yo (Put s') f) ->
      StateT $ \_ -> usingFreer k $ fmap swap $ runState s' $ f $ pure ()


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError (Freer m) = Freer $ \k -> E.runExceptT $ m $ \u ->
  case decomp u of
    Left  x -> E.ExceptT $
      usingFreer k $ liftEff $ weave (Right ()) (either (pure . Left) runError) x
    Right (Yo (Throw e) _) -> E.throwE e
    Right (Yo (Catch try handle) f) -> E.ExceptT $ do
      a <- usingFreer k $ runError $ f try
      case a of
        Right a' -> pure a
        Left e -> usingFreer k $ runError $ f $ handle e











-- runScoped :: Member (Lift IO) r => (Eff r ~> IO) -> Eff (Scoped ': r) a -> Eff r a
-- runScoped end (Eff m) = Eff $ \k -> m $ \u ->
--   case decomp u of
--     Left x -> k $ hoist (runScoped end) x
--     Right (Yo (Scoped final first) nt f) -> do
--       usingEff k $ sendM $ X.bracket
--         (end $ runScoped end $ nt $ first)
--         (\a -> end $ runScoped end $ nt $ final)
--         undefined
--       -- usingEff k $ runScoped end $ nt $ first <* final




-- emap :: (m a → m b) → (sig m a → sig m b)
-- weave
--    :: (Monad m, Monad n, Functor s)
--    => s ()
--    -> (forall x. s (m x) -> n (s x))
--     → (sig m a → sig n (s a))

