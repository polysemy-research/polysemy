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


import Control.Monad
import Data.Tuple
import Eff.Type
import Data.OpenUnion.Internal
import Data.Functor.Identity
import qualified Control.Exception as X
import           Control.Monad.Trans
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State hiding (State (..), runState)
import           Data.Functor.Compose
import           Data.Functor.Coyoneda
import           Data.Kind
import           Data.OpenUnion.Internal
import           Data.Void
import           Unsafe.Coerce


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Error e (m :: * -> *) a where
  Throw :: e -> Error e m a


data Scoped (m :: * -> *) a where
  Scoped :: m () -> m a -> Scoped m a


foo :: (Member (State String) r, Member (Error Bool) r, Member Scoped r, Member (Lift IO) r) => Eff r ()
foo = send $ Scoped (sendM $ putStrLn "DYING") $ do
  send Get >>= sendM . putStrLn
  send $ Put "works"
  send Get >>= sendM . putStrLn
  send $ Put "done"
  send $ Throw False
  sendM $ putStrLn "inside"



runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState s (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left  x -> do
      usingFreer k $ liftEff $ weave (s, ()) (uncurry runState) x

--     Right (Yo Get f) -> do
--       s' <- get
--       StateT $ \_ -> usingFreer k $ f $ pure s'
--     -- Right (Yo (Put s') f) -> f $ put s'


-- runError :: Eff (Error e ': r) a -> Eff r (Either e a)
-- runError (Eff m) = Eff $ \k -> E.runExceptT $ m $ \u ->
--   case decomp u of
--     Left  x -> E.ExceptT $ undefined
--     Right (Yo (Throw e) _ _) -> E.throwE e








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



-- main :: IO ()
-- main = (print =<<) . runM . runScoped runM . runError @Bool . runState "first" $ foo


-- emap :: (m a → m b) → (sig m a → sig m b)
-- weave
--    :: (Monad m, Monad n, Functor s)
--    => s ()
--    -> (forall x. s (m x) -> n (s x))
--     → (sig m a → sig n (s a))

