{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
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
import           Control.Monad.Trans.Identity
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
import Data.Coerce


data State s (m :: * -> *) a where
  Get :: State s m s
  Put :: s -> State s m ()


data Error e (m :: * -> *) a where
  Throw :: e -> Error e m a
  Catch :: m a -> (e -> m a) -> Error e m a


data Scoped (m :: * -> *) a where
  Scoped :: m () -> m a -> Scoped m a


data Bracket (m :: * -> *) a where
  Bracket
      :: m a
      -> (a -> m ())
      -> (a -> m r)
      -> Bracket m r


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

bar
    :: ( Member (State String) r
       , Member Scoped r
       , Member (Lift IO) r
       )
    => Eff r ()
bar = do
  send $ Scoped
    do
      sendM $ putStrLn "closing"
      send Get >>= sendM . putStrLn
      send $ Put "NO"
    do
      send Get >>= sendM . putStrLn
      send $ Put "YES"
  send Get >>= sendM . putStrLn


-- main :: IO ()
-- main = (print =<<) . runM . runError @Bool . runState "first" $ foo

-- main :: IO ()
-- main = (print =<<) . runM . runState "both" . runScoped $ bar


weave
    :: (Monad m, Monad n, Functor f)
    => (forall x. f (m x) -> n (f x))
    -> Union r m a
    -> Union r n a
weave distrib (Union w (Yo e nt)) =
  Union w $
    Yo e (fmap Compose . distrib . fmap nt . getCompose)


distribute :: Union r (StateT s (Eff r)) x -> StateT s (Eff r) x
distribute (Union w (Yo e f)) = StateT $ \s -> Freer $ \k ->
  k $ Union w $ _


-- runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r (s, a)
-- runState s = fmap swap . flip runStateT s . go
--   where
--     go :: forall x. Eff (State s ': r) x -> StateT s (Eff r) x
--     go (Freer m) = m $ \u ->
--       case decomp u of
--         Left x -> hoist go x





-- transform
--     :: forall e t r a f
--      . ( forall m. Monad m => Monad (t m)
--        , Functor f
--        )
--     => (forall x m. Functor m => t m x -> m (f x))
--     -> (forall x m. Functor m => m (f x) -> t m x)
--     -> (forall m f u v
--              . Functor f
--             => (forall x. m x -> Eff (e ': r) (f x))
--             -> (f u -> v)
--             -> e m u
--             -> t (Eff r) v
--        )
--     -> Eff (e ': r) a
--     -> Eff r (f a)
-- transform lower raise f  = lower . go
--   where
--     go :: forall x. Eff (e ': r) x -> t (Eff r) x
--     go (Freer m) = m $ \u ->
--       case decomp u of
--         Left  x -> raise . liftEff . weave lower $ hoist go x
--         Right (Yo z nt b) -> do
--           e <- f nt b z
--           go $ pure e



-- runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
-- runState s = transformSimple (fmap swap . flip runStateT s) (StateT . const . fmap swap) $
--   \case
--     Get    -> get
--     Put s' -> put s'


-- runError :: Eff (Error e ': r) a -> Eff r (Either e a)
-- runError = transform E.runExceptT E.ExceptT $ \nt b ->
--   \case
--     Throw e -> E.throwE e
--     Catch try catch -> fmap b $ E.ExceptT $ do
--       err <- runError $ nt $ try
--       case err of
--         Right a -> pure $ Right a
--         Left e -> do
--           err' <- runError $ nt $ catch e
--           case err' of
--             Left e' -> pure (Left e')
--             Right a -> pure $ Right a

