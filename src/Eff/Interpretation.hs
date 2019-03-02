{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
#if __GLASGOW_HASKELL__ >= 806
{-# LANGUAGE QuantifiedConstraints #-}
#endif
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Eff.Interpretation where

import           Control.Monad.Morph (MFunctor (..))
import           Control.Monad.Trans.Class (MonadTrans (..))
import           Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import           Data.OpenUnion.Internal
import           Eff.Type


------------------------------------------------------------------------------
-- | Interpret as an effect in terms of another effect in the stack.
natural
    :: Member eff' r
    => (forall m. eff m ~> eff' m)
    -> Eff (eff ': r) ~> Eff r
natural = naturally id
{-# INLINE natural #-}


------------------------------------------------------------------------------
-- | Interpret an effect as a monadic action in 'Eff r'.
interpret :: (forall m. eff m ~> Eff r) -> Eff (eff ': r) ~> Eff r
interpret f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x -> k x
    Right y -> runFreer (f y) k
{-# INLINE[3] interpret #-}



--------------------------------------------------------------------------------
---- | Like 'interpret', but with access to intermediate state.
--stateful
--    :: (eff ~> S.StateT s (Eff r))
--    -> s
--    -> Eff (eff ': r) a -> Eff r (a, s)
--stateful f s (Freer m) = Freer $ \k -> flip S.runStateT s $ m $ \u ->
--  case decomp u of
--    Left  x -> lift $ k x
--    Right y -> hoist (usingFreer k) $ f y
--{-# INLINE stateful #-}
---- NB: @stateful f s = transform (flip S.runStateT s) f@, but is not
---- implemented as such, since 'transform' is available only >= 8.6.0


--------------------------------------------------------------------------------
---- | Like 'interpret', but with access to intermediate state.
--withStateful
--    :: s
--    -> (eff ~> S.StateT s (Eff r))
--    -> Eff (eff ': r) a -> Eff r (a, s)
--withStateful s f = stateful f s
--{-# INLINE withStateful #-}


------------------------------------------------------------------------------
-- | Replace the topmost layer of the effect stack with another. This is often
-- useful for interpreters which would like to introduce some intermediate
-- effects before immediately handling them.
replace
    :: (forall m. eff1 m ~> eff2 m)
    -> Eff (eff1 ': r) ~> Eff (eff2 ': r)
replace = naturally weaken
{-# INLINE replace #-}


-- #if __GLASGOW_HASKELL__ >= 806
--------------------------------------------------------------------------------
---- | Run an effect via the side-effects of a monad transformer.
--transform
--    :: ( MonadTrans t
--       , forall m. Monad m => Monad (t m)
--       )
--    => (forall m. Eff r ~> m -> t (Eff r) ~> t m)
--       -- ^ The strategy for hoisting a natural transformation. This is usually
--       -- just 'hoist'.
--    -> (forall m. t m a -> m b)
--       -- ^ The strategy for getting out of the monad transformer. This is
--       -- usually just @runWhateverT@.
--    -> (eff ~> t (Eff r))
--    -> Eff (eff ': r) a
--    -> Eff r b
--transform hoist' lower f (Freer m) =
--  Freer $ \k -> lower $ m $ \u ->
--    case decomp u of
--      Left  x -> lift $ k x
--      Right y -> hoist' (usingFreer k) $ f y
--{-# INLINE[3] transform #-}
-- #endif


-- magic
--     :: Functor f
--     => f ()
--     -> (forall x . f (m x) -> n (f x))
--     ->


------------------------------------------------------------------------------
-- | Intercept an effect without removing it from the effect stack.
intercept
    :: Member eff r
    => (forall m. eff m ~> Eff r)
    -> Eff r ~> Eff r
intercept f (Freer m) = Freer $ \k -> m $ \u ->
  case prj u of
    Nothing -> k u
    Just e  -> usingFreer k $ f e
{-# INLINE intercept #-}


--------------------------------------------------------------------------------
---- | Like 'interpret', but with access to intermediate state.
--interceptS
--    :: Member eff r
--    => (eff ~> S.StateT s (Eff r))
--    -> s
--    -> Eff r a -> Eff r (a, s)
--interceptS f s (Freer m) = Freer $ \k ->
--  usingFreer k $ flip S.runStateT s $ m $ \u ->
--    case prj u of
--      Nothing -> lift $ liftEff u
--      Just e  -> f e
--{-# INLINE interceptS #-}


--------------------------------------------------------------------------------
---- | Run an effect with an explicit continuation to the final result. If you're
---- not sure why you might need this, you probably don't.
----
---- Note that this method is slow---roughly 10x slower than the other combinators
---- available here. If you just need short circuiting, consider using
---- 'shortCircuit' instead.
--relay
--    :: (a -> Eff r b)
--    -> (forall x. eff x -> (x -> Eff r b) -> Eff r b)
--    -> Eff (eff ': r) a
--    -> Eff r b
--relay pure' bind' (Freer m) = Freer $ \k ->
--  usingFreer k $ flip runContT pure' $ m $ \u ->
--    case decomp u of
--      Left  x -> lift $ liftEff x
--      Right y -> ContT $ bind' y
--{-# INLINE relay #-}


--------------------------------------------------------------------------------
---- | Like 'interpret' and 'relay'.
--interceptRelay
--    :: Member eff r
--    => (a -> Eff r b)
--    -> (forall x. eff x -> (x -> Eff r b) -> Eff r b)
--    -> Eff r a
--    -> Eff r b
--interceptRelay pure' bind' (Freer m) = Freer $ \k ->
--  usingFreer k $ flip runContT pure' $ m $ \u ->
--    case prj u of
--      Nothing -> lift $ liftEff u
--      Just y  -> ContT $ bind' y
--{-# INLINE interceptRelay #-}


------------------------------------------------------------------------------
-- | Run an effect, potentially changing the entire effect stack underneath it.
naturally
    :: Member eff' r'
    => (forall m. Union r m ~> Union r' m)
    -> (forall m. eff m ~> eff' m)
    -> Eff (eff ': r) ~> Eff r'
naturally z f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x  -> k $ z x
    Right y -> k . inj $ f y
{-# INLINE naturally #-}


------------------------------------------------------------------------------
-- | Introduce a new effect directly underneath the top of the stack. This is
-- often useful for interpreters which would like to introduce some intermediate
-- effects before immediately handling them.
--
-- Also see 'replace'.
introduce :: Eff (eff ': r) a -> Eff (eff ': u ': r) a
introduce = hoistEff intro1
{-# INLINE introduce #-}


introduce2 :: Eff (eff ': r) a -> Eff (eff ': u ': v ': r) a
introduce2 = hoistEff intro2
{-# INLINE introduce2 #-}


introduce3 :: Eff (eff ': r) a -> Eff (eff ': u ': v ': x ': r) a
introduce3 = hoistEff intro3
{-# INLINE introduce3 #-}


introduce4 :: Eff (eff ': r) a -> Eff (eff ': u ': v ':x ': y ': r) a
introduce4 = hoistEff intro4
{-# INLINE introduce4 #-}

