{-# LANGUAGE AllowAmbiguousTypes   #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Combinators
  ( -- * First order
    interpret
  , intercept
  , reinterpret
  , reinterpret2
  , reinterpret3
  , rewrite
  , transform

    -- * Higher order
  , RunH(..)
  , runH

  , interpretNew
  , interceptNew
  , reinterpretNew
  , reinterpret2New
  , reinterpret3New

    -- * Higher order with 'Tactical'
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
  , reinterpret3H

  -- * Conditional
  , interceptUsing
  , interceptUsingH
  , interceptUsingNew

    -- * Statefulness
  , stateful
  , lazilyStateful
  ) where

import           Control.Monad
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Tuple as S (swap)
import           Polysemy.Internal
import           Polysemy.Internal.CustomErrors
import           Polysemy.Internal.Tactics
import           Polysemy.Internal.Union

firstOrder
    :: ((forall rInitial x. e (Sem rInitial) x ->
         Tactical e (Sem rInitial) r x) -> t)
    -> (forall rInitial x. e (Sem rInitial) x -> Sem r x)
    -> t
firstOrder higher f = higher $ \(e :: e (Sem rInitial) x) ->
  liftT $ f e
{-# INLINE firstOrder #-}


------------------------------------------------------------------------------
-- | The simplest way to produce an effect handler. Interprets an effect @e@ by
-- transforming it into other effects inside of @r@.
interpret
    :: FirstOrder e "interpret"
    => (∀ rInitial x. e (Sem rInitial) x -> Sem r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem (e ': r) a
    -> Sem r a
-- TODO(sandy): could probably give a `coerce` impl for `runTactics` here
interpret = firstOrder interpretH
{-# INLINE interpret #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but for higher-order effects (ie. those which make use of
-- the @m@ parameter.)
--
-- 'interpretNew' is /heavily recommended/ over this. Only use 'interpretH'
-- if you need the additional power of the 'Tactical' environment -- that is,
-- the ability to inspect and manipulate the underlying effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
interpretH
    :: (∀ rInitial x . e (Sem rInitial) x -> Tactical e (Sem rInitial) r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem (e ': r) a
    -> Sem r a
interpretH f (Sem m) = Sem $ \k -> m $ \u ->
  case decomp u of
    Left  x -> k $ hoist (interpretH f) x
    Right (Weaving e mkT lwr ex) -> do
      let s = mkInitState lwr
          Distrib d = mkDistrib mkT lwr
      fmap ex $ usingSem k $ runTactics s d (interpretH f . d) $ f e
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInStateT f s (Sem sem) = Sem $ \k ->
  (S.swap <$!>) $ flip S.runStateT s $ sem $ \u ->
    case decomp u of
        Left x ->
          liftHandlerWithNat
            (\m -> S.StateT $ \s' -> S.swap <$!> interpretInStateT f s' m)
            k x
        Right (Weaving e _ lwr ex) -> do
          let z = mkInitState lwr
          ex . (<$ z) <$> S.mapStateT (usingSem k) (f e)
{-# INLINE interpretInStateT #-}


------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInLazyStateT
    :: (∀ x m. e m x -> LS.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInLazyStateT f s (Sem sem) = Sem $ \k ->
  fmap S.swap $ flip LS.runStateT s $ sem $ \u ->
    case decomp u of
        Left x ->
          liftHandlerWithNat
            (\m -> LS.StateT $ \s' -> S.swap <$> interpretInLazyStateT f s' m)
            k x
        Right (Weaving e _ lwr ex) -> do
          let z = mkInitState lwr
          ex . (<$ z) <$> LS.mapStateT (usingSem k) (f e)
{-# INLINE interpretInLazyStateT #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ (S.swap <$!>) . f e
{-# INLINE[3] stateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
lazilyStateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap S.swap . f e
{-# INLINE[3] lazilyStateful #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but for higher-order effects.
--
-- 'reinterpretNew' is /heavily recommended/ over this. Only use 'reinterpretH'
-- if you need the additional power of the 'Tactical' environment -- that is,
-- the ability to inspect and manipulate the underlying effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpretH
    :: forall e1 e2 r a
     . (∀ rInitial x. e1 (Sem rInitial) x ->
        Tactical e1 (Sem rInitial) (e2 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effect.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
reinterpretH f sem = Sem $ \k -> runSem sem $ \u ->
  case decompCoerce u of
    Left x  -> k $ hoist (reinterpretH f) $ x
    Right (Weaving e mkT lwr ex) -> do
      let s = mkInitState lwr
          Distrib d = mkDistrib mkT lwr
      fmap ex $ usingSem k
              $ runTactics s (raiseUnder . d) (reinterpretH f . d)
              $ f e
{-# INLINE[3] reinterpretH #-}
-- TODO(sandy): Make this fuse in with 'stateful' directly.


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
reinterpret
    :: forall e1 e2 r a
     . FirstOrder e1 "reinterpret"
    => (∀ rInitial x. e1 (Sem rInitial) x -> Sem (e2 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effect.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
reinterpret = firstOrder reinterpretH
{-# INLINE[3] reinterpret #-}
-- TODO(sandy): Make this fuse in with 'stateful' directly.


------------------------------------------------------------------------------
-- | Like 'reinterpret2', but for higher-order effects.
--
-- 'reinterpret2New' is /heavily recommended/ over this. Only use 'reinterpret2H'
-- if you need the additional power of the 'Tactical' environment -- that is,
-- the ability to inspect and manipulate the underlying effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpret2H
    :: forall e1 e2 e3 r a
     . (∀ rInitial x. e1 (Sem rInitial) x ->
        Tactical e1 (Sem rInitial) (e2 ': e3 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': r) a
reinterpret2H f (Sem m) = Sem $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k $ weaken $ hoist (reinterpret2H f) $ x
    Right (Weaving e mkT lwr ex) -> do
      let s = mkInitState lwr
          Distrib d = mkDistrib mkT lwr
      fmap ex $ usingSem k
              $ runTactics s (raiseUnder2 . d) (reinterpret2H f . d)
              $ f e
{-# INLINE[3] reinterpret2H #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /two/ intermediary effects.
reinterpret2
    :: forall e1 e2 e3 r a
     . FirstOrder e1 "reinterpret2"
    => (∀ rInitial x. e1 (Sem rInitial) x ->
        Sem (e2 ': e3 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': r) a
reinterpret2 = firstOrder reinterpret2H
{-# INLINE[3] reinterpret2 #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret3', but for higher-order effects.
--
-- 'reinterpret3New' is /heavily recommended/ over this. Only use 'reinterpret3H'
-- if you need the additional power of the 'Tactical' environment -- that is,
-- the ability to inspect and manipulate the underlying effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
reinterpret3H
    :: forall e1 e2 e3 e4 r a
     . (∀ rInitial x. e1 (Sem rInitial) x ->
        Tactical e1 (Sem rInitial) (e2 ': e3 ': e4 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3H f (Sem m) = Sem $ \k -> m $ \u ->
  case decompCoerce u of
    Left x  -> k . weaken . weaken . hoist (reinterpret3H f) $ x
    Right (Weaving e mkT lwr ex) -> do
      let s = mkInitState lwr
          Distrib d = mkDistrib mkT lwr
      fmap ex $ usingSem k
              $ runTactics s (raiseUnder3 . d) (reinterpret3H f . d)
              $ f e
{-# INLINE[3] reinterpret3H #-}


------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /three/ intermediary effects.
reinterpret3
    :: forall e1 e2 e3 e4 r a
     . FirstOrder e1 "reinterpret3"
    => (∀ rInitial x. e1 (Sem rInitial) x -> Sem (e2 ': e3 ': e4 ': r) x)
       -- ^ A natural transformation from the handled effect to the new effects.
    -> Sem (e1 ': r) a
    -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3 = firstOrder reinterpret3H
{-# INLINE[3] reinterpret3 #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled. This allows you, for example, to
-- intercept other effects and insert logic around them.
intercept
    :: ( Member e r
       , FirstOrder e "intercept"
       )
    => (∀ x rInitial. e (Sem rInitial) x -> Sem r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpret', 'intercept' does not consume any effects.
    -> Sem r a
intercept f = interceptH $ \(e :: e (Sem rInitial) x) ->
  liftT @(Sem rInitial) $ f e
{-# INLINE intercept #-}


------------------------------------------------------------------------------
-- | Like 'intercept', but for higher-order effects.
--
-- 'interceptNew' is /heavily recommended/ over this. Only use 'interceptH'
-- if you need the additional power of the 'Tactical' environment -- that is,
-- the ability to inspect and manipulate the underlying effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
interceptH
    :: Member e r
    => (∀ x rInitial. e (Sem rInitial) x -> Tactical e (Sem rInitial) r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpretH', 'interceptH' does not consume any effects.
    -> Sem r a
interceptH = interceptUsingH membership
{-# INLINE interceptH #-}

------------------------------------------------------------------------------
-- | A variant of 'intercept' that accepts an explicit proof that the effect
-- is in the effect stack rather then requiring a 'Member' constraint.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership'
-- in order to conditionally perform 'intercept'.
--
-- @since 1.3.0.0
interceptUsing
    :: FirstOrder e "interceptUsing"
    => ElemOf e r
       -- ^ A proof that the handled effect exists in @r@.
       -- This can be retrieved through 'Polysemy.Membership.membership' or
       -- 'Polysemy.Membership.tryMembership'.
    -> (∀ x rInitial. e (Sem rInitial) x -> Sem r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpret', 'intercept' does not consume any effects.
    -> Sem r a
interceptUsing pr f = interceptUsingH pr $ \(e :: e (Sem rInitial) x) ->
  liftT @(Sem rInitial) $ f e
{-# INLINE interceptUsing #-}

------------------------------------------------------------------------------
-- | A variant of 'interceptH' that accepts an explicit proof that the effect
-- is in the effect stack rather then requiring a 'Member' constraint.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership'
-- in order to conditionally perform 'interceptH'.
--
-- 'interceptUsingNew' is /heavily recommended/ over this. Only use
-- 'interceptUsingH' if you need the additional power of the 'Tactical'
-- environment -- that is, the ability to inspect and manipulate the underlying
-- effectful state.
--
-- See the notes on 'Tactical' for how to use this function.
--
-- @since 1.3.0.0
interceptUsingH
    :: ElemOf e r
       -- ^ A proof that the handled effect exists in @r@.
       -- This can be retrieved through 'Polysemy.Membership.membership' or
       -- 'Polysemy.Membership.tryMembership'.
    -> (∀ x rInitial. e (Sem rInitial) x -> Tactical e (Sem rInitial) r x)
       -- ^ A natural transformation from the handled effect to other effects
       -- already in 'Sem'.
    -> Sem r a
       -- ^ Unlike 'interpretH', 'interceptUsingH' does not consume any effects.
    -> Sem r a
interceptUsingH pr f (Sem m) = Sem $ \k -> m $ \u ->
  case prjUsing pr u of
    Just (Weaving e mkT lwr ex) -> do
      let s = mkInitState lwr
          Distrib d = mkDistrib mkT lwr
      fmap ex $ usingSem k
              $ runTactics s (raise . d) (interceptUsingH pr f . d)
              $ f e
    Nothing -> k $ hoist (interceptUsingH pr f) u
{-# INLINE interceptUsingH #-}

------------------------------------------------------------------------------
-- | Rewrite an effect @e1@ directly into @e2@, and put it on the top of the
-- effect stack.
--
-- @since 1.2.3.0
rewrite
    :: forall e1 e2 r a
     . (forall rInitial x. e1 (Sem rInitial) x -> e2 (Sem rInitial) x)
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
rewrite f (Sem m) = Sem $ \k -> m $ \u ->
  k $ hoist (rewrite f) $ case decompCoerce u of
    Left x -> x
    Right (Weaving e mkT lwr ex) ->
      Union Here $ Weaving (f e) mkT lwr ex


------------------------------------------------------------------------------
-- | Transform an effect @e1@ into an effect @e2@ that is already somewhere
-- inside of the stack.
--
-- @since 1.2.3.0
transform
    :: forall e1 e2 r a
     . Member e2 r
    => (forall rInitial x. e1 (Sem rInitial) x -> e2 (Sem rInitial) x)
    -> Sem (e1 ': r) a
    -> Sem r a
transform f (Sem m) = Sem $ \k -> m $ \u ->
  k $ hoist (transform f) $ case decomp u of
    Left g -> g
    Right (Weaving e mkT lwr ex) ->
      injWeaving (Weaving (f e) mkT lwr ex)


-- | An effect for running monadic actions within a higher-order effect
-- currently being interpreted.
newtype RunH z (m :: * -> *) a where
  RunH :: z a -> RunH z m a

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted.
--
-- @since TODO
runH :: Member (RunH z) r => z a -> Sem r a
runH = send . RunH

------------------------------------------------------------------------------
-- | Like 'interpret', but for higher-order effects (i.e. those which make use
-- of the @m@ parameter.)
--
-- This is significantly easier to use than 'interpretH' and its corresponding
-- 'Tactical' environment.
-- Because of this, 'interpretNew' and friends are /heavily recommended/ over
-- 'interpretH' and friends /unless/ you need the extra power that the 'Tactical'
-- environment provides -- the ability to inspect and manipulate the underlying
-- effectful state.
--
-- Higher-order thunks within the effect to be interpreted can be run using
-- 'runH'. For example:
--
-- @
-- data Bind m a where
--   Bind :: m a -> (a -> m b) -> Bind m b
--
-- runBind :: Sem (Bind ': r) a -> Sem r a
-- runBind = 'interpretNew' \\case
--   Bind ma f -> do
--     a <- 'runH' ma
--     b <- 'runH' (f a)
--     return b
-- @
--
-- @since TODO
interpretNew :: forall e r a
              . (forall z x. e z x -> Sem (RunH z ': r) x)
             -> Sem (e ': r) a
             -> Sem r a
interpretNew h (Sem sem) = Sem $ \(k :: forall x. Union r (Sem r) x -> m x) ->
  sem $ \u -> case decomp (hoist (interpretNew h) u) of
    Left g -> k g
    Right (Weaving e
                 (mkT :: forall n x
                       . Monad n
                      => (forall y. Sem r y -> n y)
                      -> z x -> t n x
                 )
                 lwr
                 ex
          ) ->
      let
          go1 :: forall x. Sem (RunH z ': r) x -> t m x
          go1 = usingSem $ \u' -> case decomp u' of
            Right (Weaving (RunH z) _ lwr' ex') ->
              (ex' . (<$ mkInitState lwr')) <$> mkT (usingSem k) z
            Left g -> liftHandlerWithNat go2 k g

          go2 :: forall x. Sem (RunH z ': r) x -> t (Sem r) x
          go2 = usingSem $ \u' -> case decomp (hoist go2 u') of
            Right (Weaving (RunH z) _ lwr' ex') ->
              (ex' . (<$ mkInitState lwr')) <$> mkT id z
            Left g -> liftHandler liftSem g
      in
        fmap ex $ lwr $ go1 (h e)

-- TODO (KingoftheHomeless): If it matters, optimize the definitions
-- below

------------------------------------------------------------------------------
-- | Like 'reinterpret', but for higher-order effects.
--
-- This is /heavily recommended/ over 'reinterpretH' unless you need
-- the extra power that the 'Tactical' environment provides.
--
-- @since TODO
reinterpretNew :: forall e1 e2 r a
                . (forall z x. e1 z x -> Sem (RunH z ': e2 ': r) x)
               -> Sem (e1 ': r) a
               -> Sem (e2 ': r) a
reinterpretNew h = interpretNew h . raiseUnder
{-# INLINE reinterpretNew #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret2', but for higher-order effects.
--
-- This is /heavily recommended/ over 'reinterpret2H' unless you need
-- the extra power that the 'Tactical' environment provides.
--
-- @since TODO
reinterpret2New :: forall e1 e2 e3 r a
                 . (forall z x. e1 z x -> Sem (RunH z ': e2 ': e3 ': r) x)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': r) a
reinterpret2New h = interpretNew h . raiseUnder2
{-# INLINE reinterpret2New #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret3', but for higher-order effects.
--
-- This is /heavily recommended/ over 'reinterpret3H' unless you need
-- the extra power that the 'Tactical' environment provides.
--
-- @since TODO
reinterpret3New :: forall e1 e2 e3 e4 r a
                 . (forall z x. e1 z x -> Sem (RunH z ': e2 ': e3 ': e4 ': r) x)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3New h = interpretNew h . raiseUnder3 
{-# INLINE reinterpret3New #-}

------------------------------------------------------------------------------
-- | Like 'intercept', but for higher-order effects.
--
-- This is /heavily recommended/ over 'interceptH' unless you need
-- the extra power that the 'Tactical' environment provides.
--
-- @since TODO
interceptNew :: forall e r a
              . Member e r
             => (forall z x. e z x -> Sem (RunH z ': r) x)
             -> Sem r a
             -> Sem r a
interceptNew h = interpretNew h . expose
{-# INLINE interceptNew #-}

------------------------------------------------------------------------------
-- | Like 'interceptUsing', but for higher-order effects.
--
-- This is /heavily recommended/ over 'interceptUsingH' unless you need
-- the extra power that the 'Tactical' environment provides.
--
-- @since TODO
interceptUsingNew :: forall e r a
                   . ElemOf e r
                  -> (forall z x. e z x -> Sem (RunH z ': r) x)
                  -> Sem r a
                  -> Sem r a
interceptUsingNew pr h = interpretNew h . exposeUsing pr
{-# INLINE interceptUsingNew #-}
