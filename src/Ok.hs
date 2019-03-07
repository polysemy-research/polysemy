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

main :: IO ()
main = (print =<<) . runM . runState "both" . runScoped $ bar


weave
    :: (Monad m, Functor f)
    => (forall x. t m x -> m (f x))
    -> Union r (t m) a
    -> Union r m (f a)
weave distrib (Union w (Yo e nt f)) =
  Union w $
    Yo e (fmap Compose . distrib . nt)
         (fmap f . getCompose)


translateSimple
    :: forall e r a
     . (forall m. e m ~> Eff r)
    -> Eff (e ': r) a
    -> Eff r a
translateSimple f
    = fmap runIdentity
    . transformSimple
          (fmap Identity . runIdentityT)
          (IdentityT . fmap runIdentity)
          (IdentityT . f)


runScoped :: Eff (Scoped ': r) a -> Eff r a
runScoped = translate $ \nt b -> \case
  Scoped after before -> fmap b $ do
    runScoped (nt before) <* runScoped (nt after)


runBracket
    :: Member (Lift IO) r
    => (Eff r ~> IO)
    -> Eff (Bracket ': r) a
    -> Eff r a
runBracket finish = translate $ \nt b -> \case
  Bracket alloc dealloc use -> sendM $ do
    X.bracket
      (finish $ runBracket finish $ nt alloc)
      (\a -> _ $ fmap (finish . runBracket finish . nt . dealloc) a)
      undefined


translate
    :: forall e r a
     . (forall m f u v
             . Functor f
            => (forall x. m x -> Eff (e ': r) (f x))
            -> (f u -> v)
            -> e m u
            -> Eff r v
       )
    -> Eff (e ': r) a
    -> Eff r a
translate f
    = fmap runIdentity
    . transform
          (fmap Identity . runIdentityT)
          (IdentityT . fmap coerce)
          (\nt b z -> IdentityT $ f nt b z)


transform
    :: forall e t r a f
     . ( forall m. Monad m => Monad (t m)
       , Functor f
       )
    => (forall x m. Functor m => t m x -> m (f x))
    -> (forall x m. Functor m => m (f x) -> t m x)
    -> (forall m f u v
             . Functor f
            => (forall x. m x -> Eff (e ': r) (f x))
            -> (f u -> v)
            -> e m u
            -> t (Eff r) v
       )
    -> Eff (e ': r) a
    -> Eff r (f a)
transform lower raise f  = lower . go
  where
    go :: forall x. Eff (e ': r) x -> t (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left  x -> raise . liftEff . weave lower $ hoist go x
        Right (Yo z nt b) -> do
          e <- f nt b z
          go $ pure e


transformSimple
    :: forall e t r a f
     . ( forall m. Monad m => Monad (t m)
       , Functor f
       )
    => (forall x m. Monad m => t m x -> m (f x))
    -> (forall x m. Functor m => m (f x) -> t m x)
    -> (forall m. e m ~> t (Eff r))
    -> Eff (e ': r) a
    -> Eff r (f a)
transformSimple lower raise f  = lower . go
  where
    go :: forall x. Eff (e ': r) x -> t (Eff r) x
    go (Freer m) = m $ \u ->
      case decomp u of
        Left  x -> raise . liftEff . weave lower $ hoist go x
        Right (Yo (z) nt b) -> do
          e <- f z
          go $ fmap b $ nt $ pure e


runState :: s -> Eff (State s ': r) a -> Eff r (s, a)
runState s = transformSimple (fmap swap . flip runStateT s) (StateT . const . fmap swap) $
  \case
    Get    -> get
    Put s' -> put s'


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError = transform E.runExceptT E.ExceptT $ \nt b ->
  \case
    Throw e -> E.throwE e
    Catch try catch -> fmap b $ E.ExceptT $ do
      err <- runError $ nt $ try
      case err of
        Right a -> pure $ Right a
        Left e -> do
          err' <- runError $ nt $ catch e
          case err' of
            Left e' -> pure (Left e')
            Right a -> pure $ Right a

