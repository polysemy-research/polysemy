{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
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
{-# OPTIONS_GHC -Wall                   #-}

module Ok where


import qualified Control.Exception as X
import           Control.Monad
import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.State hiding (State, runState)
import           Data.Functor.Compose
import           Data.OpenUnion.Internal
import           Data.Tuple
import           Eff.Type


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
      void . send $ Throw False
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
       , Member Bracket r
       , Member (Error Bool) r
       , Member (Lift IO) r
       )
    => Eff r ()
bar = do
  res <- send $ Catch
    do
      send $ Bracket
        do
          send Get <* send (Put "allocated")
        ( \a -> do
            sendM $ putStrLn $ "deallocing: " ++ a
            send Get >>= sendM . putStrLn
        )
        ( \a -> do
            sendM $ putStrLn $ "using: " ++ a
            send Get >>= sendM . putStrLn
            void . send $ Throw False
            send $ Put "used"
            pure True
        )
    \(_ :: Bool) -> do
      sendM $ putStrLn "fucking caught it!"
      pure False
  sendM $ putStrLn $ show res
  send Get >>= sendM . putStrLn
    -- \a -> do
    --   send Get >>= sendM . putStrLn
    --   send $ Put "YES"


-- main :: IO ()
-- main = (print =<<) . runM . runState "first" . runError @Bool $ foo >> foo

-- main :: IO ()
-- main = (print =<<) . runM . runState "first" . runState True $ send (Put "changed") >> send (Put False) >> send Get >>= sendM . putStrLn

main :: IO ()
main = (print =<<)
     . runM
     . runBracket runM
     . runState "both"
     . runError @Bool
     $ bar


runBracket
    :: forall r a
     . Member (Lift IO) r
    => (Eff r ~> IO)
    -> Eff (Bracket ': r) a
    -> Eff r a
runBracket finish = interpret $ \start continue -> \case
  Bracket alloc dealloc use -> sendM $
    X.bracket
      (finish $ start alloc)
      (finish . continue dealloc)
      (finish . continue use)


interpret
    :: (forall m tk
           . Functor tk
          => (m ~> Eff r .: tk)
          -> (forall a b. (a -> m b) -> tk a -> Eff r (tk b))
          -> e m
          ~> Eff r .: tk
       )
    -> Eff (e ': r)
    ~> Eff r
interpret f (Freer m) = m $ \u ->
  case decomp u of
    Left  x -> liftEff $ hoist (interpret f) x
    Right (Yo e tk nt z) -> fmap z $
      f (interpret f . nt . (<$ tk))
        (\ff -> interpret f . nt . fmap ff)
        e


weave
    :: (Monad m, Monad n, Functor f)
    => f ()
    -> (forall x. f (m x) -> n (f x))
    -> Union r m a
    -> Union r n (f a)
weave s' distrib (Union w (Yo e s nt f)) =
  Union w $
    Yo e (Compose $ s <$ s')
         (fmap Compose . distrib . fmap nt . getCompose)
         (fmap f . getCompose)


-- shundle ::


-- thread
--     :: Functor s
--     => (forall m tk y
--            . Functor tk
--           => (forall x. m x -> Eff r (s (tk x)))
--           -> (forall a b. (a -> m b) -> s (tk a) -> Eff r (s (tk b)))
--           -> e m y
--           -> Eff r (s (tk y))
--        )
--     -> s ()
--     -> Eff (e ': r) a
--     -> Eff r (s a)
-- thread f tk (fmap (<$ tk) -> Freer m) = m $ \u ->
--   case decomp u of
--     Left  x -> liftEff $ _  $ hoist (thread f tk) x
--     -- Right (Yo e tk nt z) -> fmap z $
--     --   f (thread f . nt . (<$ tk))
--     --     (\ff -> thread f . nt . fmap ff)
--     --     e


interpretLift
    :: (e ~> Eff r)
    -> Eff (Lift e ': r)
    ~> Eff r
interpretLift f (Freer m) = m $ \u ->
  case decomp u of
    Left  x -> liftEff $ hoist (interpretLift f) x
    Right (Yo (Lift e) tk _ z) ->
      fmap (z . (<$ tk)) $ f e


runState :: forall s r a. s -> Eff (State s ': r) a -> Eff r (s, a)
runState s = fmap swap . flip runStateT s . go
  where
    go :: forall x. Eff (State s ': r) x -> StateT s (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left x -> StateT $ \s' ->
          fmap swap . liftEff
                    . weave (s', ())
                            (uncurry ((fmap swap .) . flip runStateT))
                    $ hoist go x
        Right (Yo Get sf nt f) -> do
          s' <- get
          go $ fmap f $ nt $ pure s' <$ sf
        Right (Yo (Put s') sf nt f) -> do
          put s'
          go $ fmap f $ nt $ pure () <$ sf


runError :: forall e r a. Eff (Error e ': r) a -> Eff r (Either e a)
runError = E.runExceptT . go
  where
    go :: forall x. Eff (Error e ': r) x -> E.ExceptT e (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left x -> E.ExceptT  $
          liftEff . weave (Right ()) (either (pure . Left) E.runExceptT)
                  $ hoist go x
        Right (Yo (Throw e) _ _ _) -> E.throwE e
        Right (Yo (Catch try handle) sf nt f) -> fmap f $ E.ExceptT $ do
          ma <- runError $ nt $ (try <$ sf)
          case ma of
            Right _ -> pure ma
            Left e -> do
              runError $ nt $ (handle e <$ sf)

