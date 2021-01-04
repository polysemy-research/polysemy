module Polysemy.Internal.InterpretNew where

import           Polysemy.Internal
import           Polysemy.Internal.WeaveClass
import           Polysemy.Internal.Union
import           Polysemy.Internal.Kind


-- | An effect for running monadic actions within a higher-order effect
-- currently being interpreted.
data RunH z t r :: Effect where
  RunH        :: forall z t r m a. z a -> RunH z t r m a
  RunExposeH  :: forall z t r m a. z a -> RunH z t r m (t a)
  RunExposeH' :: forall z t r m a. z a -> RunH z t r m (Sem r (t a))
  RestoreH    :: forall z t r m a. t a -> RunH z t r m a

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, and recursively apply the current interpreter on it.
--
-- This is the standard tool for interpreting higher-order effects.
--
-- @since TODO
runH :: forall z t r r' a. z a -> Sem (RunH z t r ': r') a
runH = send . RunH @z @t @r
{-# INLINE runH #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted.
--
-- Unlike 'runH', this doesn't recursively apply the current interpreter
-- to the monadic action -- allowing you to run a different interpreter
-- on it instead.
--
-- @since TODO
runH' :: forall z t e r a. z a -> Sem (e ': RunH z t (e ': r) ': r) a
runH' z = runExposeH' z >>= raise . restoreH
{-# INLINE runH' #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, recursively apply the current interpreter on it,
-- and reify the effectful state of all local effects
-- as part of the result.
--
-- By reifying the effectful state, you may do one or more of the following:
--
-- * Guarantee that the handler won't be interrupted by a local effect failing,
--   since that failure will instead be reified into the state.
-- * Check if the action run has failed because of a local effect by using 'Data.Foldable.null'.
-- * Discard any impact the monadic action has on local effects by never restoring the
--   efectful state.
--
-- Once an effectful state has been reified, you may restore it using 'restoreH'.
--
-- @since TODO
runExposeH :: forall z t r r' a. z a -> Sem (RunH z t r ': r') (t a)
runExposeH = send . RunExposeH @z @t @r
{-# INLINE runExposeH #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, and reify the effectful state of all local effects
-- as part of the result.
--
-- See 'runExposeH' for more information.
--
-- Unlike 'runH', this doesn't recursively apply the current interpreter
-- to the monadic action -- allowing you to run a different interpreter
-- on it instead.
--
-- @since TODO
runExposeH' :: forall z t e r a. z a -> Sem (e ': RunH z t (e ': r) ': r) (t a)
runExposeH' z = raise (send (RunExposeH' @_ @t z)) >>= raiseUnder
{-# INLINE runExposeH' #-}

-- Restore a reified effectful state, bringing its changes into scope, and returning
-- the result of the computation.
--
-- /Note/: this overrides the local effectful state of any previously restored effectful state.
--
-- For example, consider:
--
-- @
-- 'ta' <- runExposeH ma
-- 'tb' <- runExposeH mb
-- 'restoreH' ta
-- 'restoreH' tb
-- @
-- Unless @'restoreH' ta@ causes the handler to fail (because @ma@ failed due to a local effect),
-- the changes it brings into scope will be overridden by @'restoreH' tb@.
--
-- If you want to integrate the results of both actions, you need to restore the state
-- in between uses of 'runExposeH', so that @'runExposeH' mb@ works with the changes of @ta@
-- in scope.
-- @
-- 'ta' <- runExposeH ma
-- 'restoreH' ta
-- 'tb' <- runExposeH mb
-- 'restoreH' tb
-- @
restoreH :: forall z t r r' a. t a -> Sem (RunH z t r ': r') a
restoreH = send . RestoreH @z @t @r
{-# INLINE restoreH #-}

type EffHandlerH e r =
     forall z t x
   . Traversable t
  => e z x -> Sem (RunH z t (e ': r) ': r) x

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
              . EffHandlerH e r
             -> Sem (e ': r) a
             -> Sem r a
interpretNew h (Sem sem) = Sem $ \(k :: forall x. Union r (Sem r) x -> m x) ->
  sem $ \u -> case decomp u of
    Left g -> k $ hoist (interpretNew h) g
    Right (Weaving e
                 (mkT :: forall n x
                       . Monad n
                      => (forall y. Sem (e ': r) y -> n y)
                      -> z x -> t n x
                 )
                 lwr
                 ex
          ) ->
      let
          go1 :: forall x. Sem (RunH z (StT t) (e ': r) ': r) x -> t m x
          go1 = usingSem $ \u' -> case decomp u' of
            Left g -> liftHandlerWithNat go2 k g
            Right (Weaving eff _ lwr' ex') -> do
              (ex' . (<$ mkInitState lwr')) <$> case eff of
                RunH z ->
                  mkT (usingSem k . interpretNew h) z
                RunExposeH z ->
                  liftWith $ \lower -> lower (mkT (usingSem k . interpretNew h) z)
                RunExposeH' z ->
                  liftWith $ \lower -> return (lower (mkT id z))
                RestoreH t ->
                  restoreT (return t)

          go2 :: forall x. Sem (RunH z (StT t) (e ': r) ': r) x -> t (Sem r) x
          go2 = usingSem $ \u' -> case decomp u' of
            Left g -> liftHandlerWithNat go2 liftSem g
            Right (Weaving eff _ lwr' ex') -> do
              (ex' . (<$ mkInitState lwr')) <$> case eff of
                RunH z ->
                  mkT (interpretNew h) z
                RunExposeH z ->
                  liftWith $ \lower -> lower (mkT (interpretNew h) z)
                RunExposeH' z ->
                  liftWith $ \lower -> return (lower (mkT id z))
                RestoreH t ->
                  restoreT (return t)
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
                . EffHandlerH e1 (e2 ': r)
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
                 . EffHandlerH e1 (e2 ': e3 ': r)
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
                 . EffHandlerH e1 (e2 ': e3 ': e4 ': r)
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
             => EffHandlerH e r
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
                  -> EffHandlerH e r
                  -> Sem r a
                  -> Sem r a
interceptUsingNew pr h = interpretNew h . exposeUsing pr
{-# INLINE interceptUsingNew #-}
