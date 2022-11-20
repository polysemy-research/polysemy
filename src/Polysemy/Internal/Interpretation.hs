{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Polysemy.Internal.Interpretation where

import Control.Monad

import Polysemy.Internal
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Kind
import Polysemy.Internal.Union


newtype Processor z t r = Processor { getProcessor :: forall x. z x -> Sem r (t x) }


-- | An effect for running monadic actions within a higher-order effect
-- currently being interpreted.
data RunH z t e r :: Effect where
  RunH             :: forall z t e r m a
                    . z a
                   -> RunH z t e r m a
  WithProcessorH   :: forall z t e r m a
                    . ((forall x. z x -> Sem (e ': r) (t x)) -> a)
                   -> RunH z t e r m a
  WithInterpreterH :: forall z t e r m a
                    . ((forall x. Sem (e ': r) x -> Sem r x) -> a)
                   -> RunH z t e r m a
  LiftWithH       :: forall z t e r r' m a
                    . (forall x. Sem (e ': r) x -> Sem r' x)
                   -> ((forall x. Sem (RunH z t e r ': r') x -> Sem r' (t x)) -> a)
                   -> RunH z t e r m a
  RestoreH         :: forall z t e r m a
                    . t a
                   -> RunH z t e r m a

propagate :: forall e r rInitial t e' r' a
           . Member e r
          => e (Sem rInitial) a
          -> Sem (RunH (Sem rInitial) t e' r' ': r) a
propagate e = liftSem $ hoist runH $ Union (There membership) (mkWeaving e)
{-# INLINE propagate #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, and recursively apply the current interpreter on it.
--
-- This is the standard tool for interpreting higher-order effects.
--
-- @since TODO
runH :: forall z t e r r' a. z a -> Sem (RunH z t e r ': r') a
runH = send . RunH @z @t @e @r
{-# INLINE runH #-}

liftWithH :: forall z t e r r' a
           . ((forall x. Sem (RunH z t e r ': r) x -> Sem r (t x)) -> Sem r' a)
          -> Sem (RunH z t e r ': r') a
liftWithH main = withInterpreterH $ \interp -> liftWithH' interp main
{-# INLINE liftWithH #-}

liftWithH' :: forall z t e r r' r'' a
            . (forall x. Sem (e ': r) x -> Sem r' x)
           -> ((forall x. Sem (RunH z t e r ': r') x -> Sem r' (t x))
               -> Sem r'' a)
           -> Sem (RunH z t e r ': r'') a
liftWithH' interp main = send (LiftWithH interp main) >>= raise
{-# INLINE liftWithH' #-}

withInterpreterH :: forall z t e r r' a
                  . (   (forall x. Sem (e ': r) x -> Sem r x)
                     -> Sem (RunH z t e r ': r') a
                    )
                 -> Sem (RunH z t e r ': r') a
withInterpreterH main = join $ send (WithInterpreterH @z @t main)

controlH :: forall z t e r r' a
          . (   (forall x. Sem (RunH z t e r ': r) x -> Sem r (t x))
             -> Sem r' (t a)
            )
         -> Sem (RunH z t e r ': r') a
controlH main = liftWithH main >>= restoreH @z @t @e @r
{-# INLINE controlH #-}

controlH' :: forall z t e r r' r'' a
           . (forall x. Sem (e ': r) x -> Sem r' x)
          -> ((forall x. Sem (RunH z t e r ': r') x -> Sem r' (t x))
              -> Sem r'' (t a))
          -> Sem (RunH z t e r ': r'') a
controlH' interp main = liftWithH' interp main >>= restoreH @z @t @e @r
{-# INLINE controlH' #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted.
--
-- Unlike 'runH', this doesn't recursively apply the current interpreter
-- to the monadic action -- allowing you to run a different interpreter
-- on it instead.
--
-- @since TODO
runH' :: forall z t e r a. z a -> Sem (e ': RunH z t e r ': r) a
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
-- * Check if the action run has failed because of a local effect by using
--   'Data.Foldable.null' or @`Data.Traversable.traverse` (const Nothing)@.
-- * Discard any impact the monadic action has on local effects by never
--   restoring the efectful state.
--
-- Once an effectful state has been reified, you may restore it using 'restoreH'.
--
-- @since TODO
runExposeH :: forall z t e r a. z a -> Sem (RunH z t e r ': r) (t a)
runExposeH z = withInterpreterH $ \n -> do
  Processor pr <- getProcessorH
  raise (n (pr z))
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
runExposeH' :: forall z t e r a. z a -> Sem (e ': RunH z t e r ': r) (t a)
runExposeH' z = do
  Processor pr <- raise getProcessorH
  raiseUnder (pr z)
{-# INLINE runExposeH' #-}



-- | Restore a reified effectful state, bringing its changes into scope, and
-- returning the result of the computation.
--
-- /Note/: this overrides the local effectful state of any previously restored
-- effectful state.
--
-- For example, consider:
--
-- @
-- 'ta' <- runExposeH ma
-- 'tb' <- runExposeH mb
-- 'restoreH' ta
-- 'restoreH' tb
-- @
-- Unless @'restoreH' ta@ causes the handler to fail (because @ma@ failed due to
-- a local effect), the changes it brings into scope will be overridden by
-- @'restoreH' tb@.
--
-- If you want to integrate the results of both actions, you need to restore the
-- state in between uses of 'runExposeH', so that @'runExposeH' mb@ works with
-- the changes of @ta@ in scope.
-- @
-- 'ta' <- runExposeH ma
-- 'restoreH' ta
-- 'tb' <- runExposeH mb
-- 'restoreH' tb
-- @
--
-- @since TODO
restoreH :: forall z t e r r' a. t a -> Sem (RunH z t e r ': r') a
restoreH = send . RestoreH @z @_ @e @r
{-# INLINE restoreH #-}

-- | Reify the effectful state of the local effects of the argument.
--
-- @'runExposeH' m = 'exposeH' ('runH' m)@
--
-- /Note/: `polysemy-plugin` is heavily recommended when using this function
-- to avoid type ambiguous types. If `polysemy-plugin` isn't available, consider
-- using 'runExposeH' and `runExposeH'` instead.
--
-- @since TODO
exposeH :: forall z t e r a
         . Sem (RunH z t e r ': r) a
        -> Sem (RunH z t e r ': r) (t a)
exposeH m = liftWithH $ \lower -> lower m
{-# INLINE exposeH #-}

-- | Retrieve a 'Processor': a function which can be used to process a monadic
-- action given by a higher-order effect that is currently being interpreted
-- without immediately running it, turning it into a @'Sem' (e ': r)@ action
-- that returns a reified effectful state.
getProcessorH :: forall z t e r r'
               . Sem (RunH z t e r ': r') (Processor z t (e ': r))
getProcessorH = send (WithProcessorH @_ @_ @e Processor)
{-# INLINE getProcessorH #-}

type Tactical z e r r' a =
      forall t
    . Traversable t
  => Sem (RunH z t e r ': r') a

type EffHandlerH e r =
     forall rInitial x. e (Sem rInitial) x -> Tactical (Sem rInitial) e r r x

------------------------------------------------------------------------------
-- | Like 'interpret', but for higher-order effects (i.e. those which make use
-- of the @m@ parameter.)
--
-- Higher-order actions within the effect to be interpreted can be run using
-- 'runH' or 'runH''. For example:
--
-- @
-- data Bind m a where
--   Bind :: m a -> (a -> m b) -> Bind m b
--
-- runBind :: Sem (Bind ': r) a -> Sem r a
-- runBind = 'interpretH' \\case
--   Bind ma f -> do
--     a <- 'runH' ma
--     b <- 'runH' (f a)
--     return b
-- @
--
-- 'interpretH' has a large suite of associated operators besides 'runH' and
-- 'runH'', which can be accessed through the "Polysemy.Tactical" module. These
-- operators are power tools only meant to be used for complex interpretations
-- of higher-order effects; 'runH' and 'runH'' are sufficient for most uses of
-- 'interpretH'.
--
-- @since TODO
interpretH :: forall e r a
              . EffHandlerH e r
             -> Sem (e ': r) a
             -> Sem r a
interpretH h (Sem sem) = Sem $ \(k :: forall x. Union r (Sem r) x -> m x) ->
  sem $ \u -> case decomp u of
    Left g -> k $ hoist (interpretH h) g
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
          {-# SPECIALIZE INLINE
              commonHandler :: forall n x
                             . Weaving (RunH z (StT t) e r) n x
                            -> t m x #-}
          {-# SPECIALIZE INLINE
              commonHandler :: forall r' n x
                             . Weaving (RunH z (StT t) e r) n x
                            -> t (Sem r') x #-}
          commonHandler :: forall n b x
                         . Monad b => Weaving (RunH z (StT t) e r) n x -> t b x
          commonHandler (Weaving eff _ lwr' ex') = do
            let run_it = fmap (ex' . (<$ mkInitState lwr'))
            case eff of
              RunH _ -> errorWithoutStackTrace "RunH not commonly handled"
              WithInterpreterH main -> run_it $ return $ main $ interpretH h
              WithProcessorH main -> run_it $
                liftWith $ \lower -> return $ main (lower . mkT id)
              RestoreH t -> run_it $
                restoreT (return t)
              LiftWithH subInterpret main -> run_it $ liftWith $ \lower ->
                return $ main (lower . (go3 subInterpret))

          go1 :: forall x. Sem (RunH z (StT t) e r ': r) x -> t m x
          go1 = usingSem $ \u' -> case decomp u' of
            Left g -> liftHandlerWithNat go2 k g
            Right wav@(Weaving eff _ lwr' ex') -> do
              let run_it = (ex' . (<$ mkInitState lwr'))
              case eff of
                RunH z -> run_it <$> mkT (usingSem k . interpretH h) z
                _      -> commonHandler wav
          {-# INLINE go1 #-}

          go2 :: forall x. Sem (RunH z (StT t) e r ': r) x -> t (Sem r) x
          go2 = usingSem $ \u' -> case decomp u' of
            Left g -> liftHandlerWithNat go2 liftSem g
            Right wav@(Weaving eff _ lwr' ex') -> do
              let run_it = (ex' . (<$ mkInitState lwr'))
              case eff of
                RunH z -> run_it <$> mkT (interpretH h) z
                _      -> commonHandler wav
          {-# NOINLINE go2 #-}

          go3 :: forall r' x
               . (forall y. Sem (e ': r) y -> Sem r' y)
              -> Sem (RunH z (StT t) e r ': r') x
              -> t (Sem r') x
          go3 subInterpret = usingSem $ \u' -> case decomp u' of
            Left g -> liftHandlerWithNat (go3 subInterpret) liftSem g
            Right wav@(Weaving eff _ lwr' ex') -> do
              let run_it = (ex' . (<$ mkInitState lwr'))
              case eff of
                RunH z -> run_it <$> mkT subInterpret z
                _      -> commonHandler wav
          {-# NOINLINE go3 #-}
      in
        fmap ex $ lwr $ go1 (h e)
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | The simplest way to produce an effect handler. Interprets an effect @e@ by
-- transforming it into other effects inside of @r@.
--
-- @since TODO
interpret :: forall e r a
              . FirstOrder e "interpret"
             => (∀ rInitial x. e (Sem rInitial) x -> Sem r x)
             -> Sem (e ': r) a
             -> Sem r a
interpret h =
  interpretH $ \e -> raise (h e)
{-# INLINE interpret #-}

-- TODO (KingoftheHomeless): If it matters, optimize the definitions
-- below

------------------------------------------------------------------------------
-- | Like 'reinterpret', but for higher-order effects.
--
-- @since TODO
reinterpretH :: forall e1 e2 r a
                . EffHandlerH e1 (e2 ': r)
               -> Sem (e1 ': r) a
               -> Sem (e2 ': r) a
reinterpretH h = interpretH h . raiseUnder
{-# INLINE reinterpretH #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- @since TODO
reinterpret :: forall e1 e2 r a
              . FirstOrder e1 "reinterpret"
             => (∀ rInitial x. e1 (Sem rInitial) x -> Sem (e2 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': r) a
reinterpret h =
  reinterpretH $ \e -> raise (h e)
{-# INLINE reinterpret #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret2', but for higher-order effects.
--
-- @since TODO
reinterpret2H :: forall e1 e2 e3 r a
                 . EffHandlerH e1 (e2 ': e3 ': r)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': r) a
reinterpret2H h = interpretH h . raiseUnder2
{-# INLINE reinterpret2H #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /two/ intermediary effects.
--
-- @since TODO
reinterpret2 :: forall e1 e2 e3 r a
              . FirstOrder e1 "reinterpret2"
             => (∀ rInitial x. e1 (Sem rInitial) x -> Sem (e2 ': e3 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': e3 ': r) a
reinterpret2 h =
  reinterpret2H $ \e -> raise (h e)
{-# INLINE reinterpret2 #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret3', but for higher-order effects.
--
-- @since TODO
reinterpret3H :: forall e1 e2 e3 e4 r a
                 . EffHandlerH e1 (e2 ': e3 ': e4 ': r)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3H h = interpretH h . raiseUnder3
{-# INLINE reinterpret3H #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /three/ intermediary effects.
--
-- @since TODO
reinterpret3 :: forall e1 e2 e3 e4 r a
              . FirstOrder e1 "reinterpret3"
             => (∀ rInitial x. e1 (Sem rInitial) x -> Sem (e2 ': e3 ': e4 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3 h =
  reinterpret3H $ \e -> raise (h e)
{-# INLINE reinterpret3 #-}

------------------------------------------------------------------------------
-- | Like 'intercept', but for higher-order effects.
--
-- @since TODO
intercept :: forall e r a
              . Member e r
             => EffHandlerH e r
             -> Sem r a
             -> Sem r a
intercept h = interpretH h . expose
{-# INLINE intercept #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled. This allows you, for example, to
-- intercept other effects and insert logic around them.
--
-- @since TODO
interceptH :: forall e r a
              . FirstOrder e "intercept"
             => Member e r
             => (∀ rInitial x. e (Sem rInitial) x -> Sem r x)
             -> Sem r a
             -> Sem r a
interceptH h =
  intercept $ \e -> raise (h e)
{-# INLINE interceptH #-}

------------------------------------------------------------------------------
-- | Like 'interceptUsing', but for higher-order effects.
--
-- @since TODO
interceptUsing :: forall e r a
                   . ElemOf e r
                  -> EffHandlerH e r
                  -> Sem r a
                  -> Sem r a
interceptUsing pr h = interpretH h . exposeUsing pr
{-# INLINE interceptUsing #-}

------------------------------------------------------------------------------
-- | A variant of 'intercept' that accepts an explicit proof that the effect
-- is in the effect stack rather then requiring a 'Member' constraint.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership'
-- in order to conditionally perform 'intercept'.
--
-- @since TODO
interceptUsingH :: forall e r a .
                     FirstOrder e "interceptUsing"
                  => Member e r
                  => ElemOf e r
                  -> (∀ rInitial x. e (Sem rInitial) x -> Sem r x)
                  -> Sem r a
                  -> Sem r a
interceptUsingH pr h =
  interceptUsing pr $ \e -> raise (h e)
{-# INLINE interceptUsingH #-}
