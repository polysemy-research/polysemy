{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TypeOperators         #-}

module Eff.Combinators where

import qualified Control.Monad.Trans.Except as E
import           Control.Monad.Trans.Cont
import qualified Control.Monad.Trans.State.Strict as S
import           Data.OpenUnion
import           Eff.Type


------------------------------------------------------------------------------
-- | Interpret as an effect in terms of another effect in the stack.
natural
    :: Member eff' r
    => (eff ~> eff')
    -> Eff (eff ': r) ~> Eff r
natural = naturally id
{-# INLINE natural #-}


------------------------------------------------------------------------------
-- | Interpret an effect as a monadic action in 'Eff r'.
interpret :: (eff ~> Eff r) -> Eff (eff ': r) ~> Eff r
interpret f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x -> k x
    Right y -> runFreer (f y) k
{-# INLINE interpret #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to intermediate state.
interpretS
    :: (eff ~> S.StateT s (Eff r))
    -> s
    -> Eff (eff ': r) ~> Eff r
interpretS f s = transform (flip S.evalStateT s) f
{-# INLINE interpretS #-}


------------------------------------------------------------------------------
-- | Replace the topmost layer of the effect stack with another. This is often
-- useful for interpreters which would like to introduce some intermediate
-- effects before immediately handling them.
replace
    :: (eff ~> eff')
    -> Eff (eff ': r) ~> Eff (eff' ': r)
replace = naturally weaken
{-# INLINE replace #-}


------------------------------------------------------------------------------
-- | Run an effect via the side-effects of a monad transformer.
transform
    :: ( MonadTrans t
       , MFunctor t
       , forall m. Monad m => Monad (t m)
       )
    => (forall m. Monad m => t m a -> m b)
       -- ^ The strategy for getting out of the monad transformer.
    -> (eff ~> t (Eff r))
    -> Eff (eff ': r) a
    -> Eff r b
transform lower f (Freer m) = Freer $ \k -> lower $ m $ \u ->
  case decomp u of
    Left  x -> lift $ k x
    Right y -> hoist (runIt k) $ f y
{-# INLINE transform #-}


------------------------------------------------------------------------------
-- | Run an effect, potentially short circuiting in its evaluation.
shortCircuit
    :: (eff ~> E.ExceptT e (Eff r))
    -> Eff (eff ': r) a
    -> Eff r (Either e a)
shortCircuit f = transform E.runExceptT $ \e -> f e


------------------------------------------------------------------------------
-- | Run an effect with an explicit continuation to the final result. If you're
-- not sure why you might need this, you probably don't.
--
-- Note that this method is slow---roughly 10x slower than the other combinators
-- available here. If you just need short circuiting, consider using
-- 'shortCircuit' instead.
relay
    :: (a -> Eff r b)
    -> (forall x. eff x -> (x -> Eff r b) -> Eff r b)
    -> Eff (eff ': r) a
    -> Eff r b
relay pure' bind' (Freer m) = Freer $ \k ->
  runIt k $ flip runContT pure' $ m $ \u ->
    case decomp u of
      Left  x -> lift $ liftEff x
      Right y -> ContT $ bind' y
{-# INLINE relay #-}


------------------------------------------------------------------------------
-- | Run an effect, potentially changing the entire effect stack underneath it.
naturally
    :: Member eff' r'
    => (Union r ~> Union r')
    -> (eff ~> eff')
    -> Eff (eff ': r) ~> Eff r'
naturally z f (Freer m) = Freer $ \k -> m $ \u ->
  case decomp u of
    Left x  -> k $ z x
    Right y -> k . inj $ f y
{-# INLINE naturally #-}


------------------------------------------------------------------------------
-- | Analogous to MTL's 'lift'.
raise :: Eff r a -> Eff (u ': r) a
raise = hoistEff weaken
{-# INLINE raise #-}


------------------------------------------------------------------------------
-- | Introduce a new effect directly underneath the top of the stack. This is
-- often useful for interpreters which would like to introduce some intermediate
-- effects before immediately handling them.
--
-- Also see 'replace'.
introduce :: Eff (eff ': r) a -> Eff (eff ': u ': r) a
introduce = hoistEff intro
{-# INLINE introduce #-}

