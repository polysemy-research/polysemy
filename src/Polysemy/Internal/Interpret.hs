{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}
module Polysemy.Internal.Interpret where

import Control.Monad

import Polysemy.Internal
import Polysemy.Internal.CustomErrors (FirstOrder)
import Polysemy.Internal.Kind
import Polysemy.Internal.Union

-- A reified interpreter, transforming @'Polysemy.Sem' (e ': rPre)@ to
-- @'Polysemy.Sem' rPost@.
--
-- @since 2.0.0.0
newtype InterpreterH e rPre rPost = InterpreterH {
  unInterpreterH :: forall x. Sem (e ': rPre) x -> Sem rPost x
  }

-- | An effect providing various tools needed for higher-order interpreters,
-- most notably the ability to run higher-order chunks of the interpreted
-- effect. See 'interpretH' for a simple usage guide.
--
-- The type parameters represent the following:
--
-- * @z@: The monad of the higher-order thunks of the effect being interpreted.
-- * @t@: The type of effectful state of effects that have already been
--   interpreted (sometimes called local (effectful) state). @t@ is always
--   'Traversable', which can sometimes be useful (see
--   'Polysemy.Interpret.runExposeH' for an example.)
-- * @e@: The effect being handled
-- * @rPre@: The tail of the effect stack (i.e. omitting @e@) before the
--   interpreter is run
-- * @rPost@: The resulting effect stack after the interpreter is run
--
-- @since 2.0.0.0
data Handling z t e rPre rPost :: Effect where
  WithProcessorH
    :: forall z t e rPre rPost m a
     . ((forall x. z x -> Sem (e ': rPre) (t x)) -> a)
    -> Handling z t e rPre rPost m a
  GetInterpreterH
    :: forall z t e rPre rPost m
     . Handling z t e rPre rPost m (InterpreterH e rPre rPost)
  LiftWithH
    :: forall z t e rPre rPost m a
     . ((forall r x. Sem (Handling z t e rPre rPost ': r) x -> Sem r (t x))
        -> a)
    -> Handling z t e rPre rPost m a
  RestoreH
    :: forall z t e rPre rPost m a. t a -> Handling z t e rPre rPost m a

-- | Propagate an effect action where the higher-order chunks are of the same
-- monad @z@ as that used by the effect currently being handled.
--
-- This is useful for interpreters that want to rewrite and propagate actions.
-- For example, 'Polysemy.transform' can be written using this:
--
-- @
-- transform t = interpretH $ \e -> propagate (t e)
-- @
propagate :: forall e r z t eH rPre rPost a
           . (Member e r, Subsume rPost r)
          => e z a
          -> Sem (Handling z t eH rPre rPost ': r) a
propagate = propagateUsing membership
{-# INLINE propagate #-}

-- | Propagate an effect action where the higher-order chunks are of the same
-- monad @z@ as that used by the effect currently being handled, given an
-- explicit proof that the effect exists in the effect stack.
propagateUsing :: forall e r z t eH rPre rPost a
                . Subsume rPost r
               => ElemOf e r
               -> e z a
               -> Sem (Handling z t eH rPre rPost ': r) a
propagateUsing pr = sendViaUsing (There pr) runH
{-# INLINE propagateUsing #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, and recursively apply the current interpreter on it.
--
-- This is the standard tool for interpreting higher-order effects. It is
-- the simplest -- and most commonly useful -- way to process higher-order
-- chunks of effects.
--
-- Don't be intimidated by the signature; it looks the way it is to allow
-- 'runH' to be used in as many contexts as possible, without requiring
-- any type applications to get it to work.
--
-- In the case of 'Polysemy.interpretH', @r@, @rPre@, and @rPost@ are all the
-- same, making the signature the more understandable:
--
-- @
-- 'runH' :: z a -> 'Polysemy.Sem' ('Polysemy.Handling' z t e r r ': r) a
-- @
--
-- @since 2.0.0.0
runH :: forall z t e rPre rPost r a
      . Subsume rPost r
     => z a
     -> Sem (Handling z t e rPre rPost ': r) a
runH = runExposeH >=> restoreH
{-# INLINE runH #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted.
--
-- Unlike 'runH', this doesn't recursively apply the current interpreter
-- to the monadic action -- allowing you to run a different interpreter
-- on it instead.
--
-- @since 2.0.0.0
runH' :: forall z t e rPre rPost r a
       . Subsume rPre r
      => z a
      -> Sem (e ': Handling z t e rPre rPost ': r) a
runH' = runExposeH' >=> raise . restoreH
{-# INLINE runH' #-}

-- | Locally gain access to lowering function that can transform
-- @'Polysemy.Sem' ('Polysemy.Interpretation.Handling' z t ... ': r) x@ to
-- @'Polysemy.Sem' r (t x)@.
--
-- This is analogous to @liftWith@ of @MonadTransControl@.
--
-- Note: the lowering function lowers @'Sem' ('Handling' ... ': r)@ by using the
-- effectful state as it is when 'liftWithH' is run.
liftWithH :: forall z t e rPre rPost r a
           . (   (   forall r' x
                   . Sem (Handling z t e rPre rPost ': r') x
                  -> Sem r' (t x))
              -> Sem r a)
          -> Sem (Handling z t e rPre rPost ': r) a
liftWithH main = sendUsing Here (LiftWithH main) >>= raise
{-# INLINE liftWithH #-}

-- | A particularly useful composition:
-- @'controlH' h = 'liftWithH' h >>= 'restoreH'@
--
-- This is analogous to @controlT@ of @MonadTransControl@.
--
-- Note: the lowering function lowers @'Sem' ('Handling' ... ': r)@ by using the
-- effectful state as it is when 'controlH' is run.
controlH :: forall z t e rPre rPost r a
          . (   (   forall r' x
                  . Sem (Handling z t e rPre rPost ': r') x
                 -> Sem r' (t x))
             -> Sem r (t a))
         -> Sem (Handling z t e rPre rPost ': r) a
controlH main = liftWithH main >>= restoreH
{-# INLINE controlH #-}

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
-- Once an effectful state has been reified, you may restore it using
-- 'restoreH'.
--
-- @since TODO
runExposeH :: forall z t e rPre rPost r a
            . Subsume rPost r
           => z a -> Sem (Handling z t e rPre rPost ': r) (t a)
runExposeH z = do
  InterpreterH interp <- getInterpreterH
  withProcessorH $ \prcs -> subsume_ $ interp $ prcs z
{-# INLINE runExposeH #-}

-- | Run a monadic action given by a higher-order effect that is currently
-- being interpreted, and reify the effectful state of all local effects
-- as part of the result.
--
-- See 'runExposeH' for more information.
--
-- Unlike 'runExposeH', this doesn't recursively apply the current interpreter
-- to the monadic action -- allowing you to run a different interpreter
-- on it instead.
--
-- @since TODO
runExposeH' :: forall z t e rPre rPost r a
             . Subsume rPre r
            => z a
            -> Sem (e ': Handling z t e rPre rPost ': r) (t a)
runExposeH' = raise . processH >=> subsume_
{-# INLINE runExposeH' #-}


-- | Restore a reified effectful state, bringing its changes into scope, and
-- returning the result of the computation.
--
-- This is analogous to @restoreT . return@ of @MonadTransControl@ or
-- @restoreM@ of @MonadBaseControl@.
--
-- /Note/: this overrides the local effectful state of any previously restored
-- effectful state.
--
-- For example, consider:
--
-- @
-- ta <- 'runExposeH' ma
-- tb <- 'runExposeH' mb
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
-- ta <- 'runExposeH' ma
-- 'restoreH' ta
-- tb <- 'runExposeH' mb
-- 'restoreH' tb
-- @
--
-- @since TODO
restoreH :: forall z t e rPre rPost r a
          . t a -> Sem (Handling z t e rPre rPost ': r) a
restoreH = sendUsing Here . RestoreH
{-# INLINE restoreH #-}

-- | Reify the effectful state of the local effects of the argument.
--
-- @'runExposeH' m = 'exposeH' ('runH' m)@
--
-- @since TODO
exposeH :: forall z t e rPre rPost r a
         . Sem (Handling z t e rPre rPost ': r) a
        -> Sem (Handling z t e rPre rPost ': r) (t a)
exposeH m = liftWithH $ \lower -> lower m
{-# INLINE exposeH #-}

-- | Process a monadic action given by the higher-order effect that is currently
-- being interpreted by turning it into a @'Sem' (e ': rPre)@ action that
-- returns a reified effectful state. The processed monadic action is returned,
-- rather than being immediately run like with 'runH''.
--
-- /Note/: The processed action makes use of the effectful state as it is by
-- the time 'processH' is run, rather than what it is by the time the processed
-- action is run.
processH :: forall z t e rPre rPost r a
          . z a
         -> Sem (Handling z t e rPre rPost ': r) (Sem (e ': rPre) (t a))
processH z = withProcessorH $ \lower -> return (lower z)
{-# INLINE processH #-}

-- | Locally gain access to a processor: a function that transforms a monadic
-- action given by the higher-order effect that is currently being interpreted
-- by turning it into a @'Sem' (e ': rPre)@ action that returns a reified
-- effectful state.
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'withProcessorH' is run, rather than what it is by the time the
-- processed action is run.
withProcessorH :: forall z t e rPre rPost r a
                . ((forall x. z x -> Sem (e ': rPre) (t x))
                   -> Sem r a)
               -> Sem (Handling z t e rPre rPost ': r) a
withProcessorH main = sendUsing Here (WithProcessorH main) >>= raise
{-# INLINE withProcessorH #-}

-- | A particularly useful composition:
-- @'controlWithProcessorH' h = 'withProcessorH' h >>= 'restoreH'@
--
-- /Note/: Processed actions makes use of the effectful state as it is by
-- the time 'withProcessorH' is run, rather than what it is by the time the
-- processed action is run.
controlWithProcessorH :: forall z t e rPre rPost r a
                       . ((forall x. z x -> Sem (e ': rPre) (t x))
                          -> Sem r (t a))
                      -> Sem (Handling z t e rPre rPost ': r) a
controlWithProcessorH main = withProcessorH main >>= restoreH
{-# INLINE controlWithProcessorH #-}

-- | Retrieve a 'InterpreterH': the interpreter currently being defined
getInterpreterH :: forall z t e rPre rPost r
                 . Sem (Handling z t e rPre rPost ': r)
                       (InterpreterH e rPre rPost)
getInterpreterH = sendUsing Here GetInterpreterH
{-# INLINE getInterpreterH #-}

-- | A handler for a higher-order effect @e@, working with the effect stack
-- @rPost@.
type EffHandlerH e rPre rPost =
       forall t z x
     . Traversable t
    => e z x -> Sem (Handling z t e rPre rPost ': rPost) x

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
-- 'runH'', which can be accessed through the "Polysemy.Interpretation" module.
-- These operators are power tools only meant to be used for complex
-- interpretations of higher-order effects; 'runH' and 'runH'' are sufficient
-- for most uses of 'interpretH'.
--
-- @since TODO
interpretH :: forall e r a
              . EffHandlerH e r r
             -> Sem (e ': r) a
             -> Sem r a
interpretH = genericInterpretH
{-# INLINE interpretH #-}

------------------------------------------------------------------------------
-- | A generalization of 'interpretH', 'reinterpretH', 'reinterpret2H', etc.:
-- given a handler @h@ for a (higher-order) effect @e@, @'genericInterpretH' h@
-- gives an interpreter @Sem (e ': r)@ to @Sem r'@, as long @r@ can be
-- transformed to @r'@ through subsuming and/or introducing effects.
--
-- Due to its generic nature, 'genericInterpretH' is often unsuitable for inline
-- use. It's most useful to defined specializations (like 'interpretH' and
-- 'reinterpretH'.)
--
-- @since TODO
genericInterpretH :: forall e r' r a
                  . Subsume r r'
                 => EffHandlerH e r r'
                 -> Sem (e ': r) a
                 -> Sem r' a
genericInterpretH h = go
  where
    go :: forall a'. Sem (e ': r) a' -> Sem r' a'
    go (Sem sem) = Sem $ \(k :: forall x. Union r' (Sem r') x -> m x) ->
      sem $ \u -> case decomp u of
        Left g -> k $ subsumeUnion @r @r' $ hoist go g
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
                                 . Weaving (Handling z (StT t) e r r') n x
                                -> t m x #-}
              {-# SPECIALIZE INLINE
                  commonHandler :: forall r'' n x
                                 . Weaving (Handling z (StT t) e r r') n x
                                -> t (Sem r'') x #-}
              commonHandler :: forall n b x
                             . Monad b
                            => Weaving (Handling z (StT t) e r r') n x -> t b x
              commonHandler (Weaving eff _ lwr' ex') = do
                let run_it = fmap (ex' . (<$ mkInitState lwr'))
                case eff of
                  GetInterpreterH -> run_it $ return $ InterpreterH go
                  WithProcessorH main -> run_it $
                    liftWith $ \lower -> return $ main (lower . mkT id)
                  RestoreH t -> run_it $
                    restoreT (return t)
                  LiftWithH main -> run_it $ liftWith $ \lower ->
                    return $ main (lower . go3)

              go1 :: forall x. Sem (Handling z (StT t) e r r' ': r') x -> t m x
              go1 = usingSem $ \u' -> case decomp u' of
                Left g -> liftHandlerWithNat go2 k g
                Right wav -> commonHandler wav
              {-# INLINE go1 #-}

              go2 :: forall x
                   . Sem (Handling z (StT t) e r r' ': r') x -> t (Sem r') x
              go2 = usingSem $ \u' -> case decomp u' of
                Left g -> liftHandlerWithNat go2 liftSem g
                Right wav -> commonHandler wav
              {-# NOINLINE go2 #-}

              go3 :: forall r'' x
                   . Sem (Handling z (StT t) e r r' ': r'') x
                  -> t (Sem r'') x
              go3 = usingSem $ \u' -> case decomp u' of
                Left g -> liftHandlerWithNat go3 liftSem g
                Right wav -> commonHandler wav
              {-# NOINLINE go3 #-}
          in
            fmap ex $ lwr $ go1 (h e)
{-# INLINE genericInterpretH #-}

------------------------------------------------------------------------------
-- | The simplest way to produce an effect handler. Interprets an effect @e@ by
-- transforming it into other effects inside of @r@.
--
-- @since TODO
interpret :: forall e r a
              . FirstOrder e "interpret"
             => (∀ z x. e z x -> Sem r x)
             -> Sem (e ': r) a
             -> Sem r a
interpret h =
  interpretH $ \e -> raise (h e)
{-# INLINE interpret #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret', but for higher-order effects. See 'interpretH' for a
-- simple usage guide.
--
-- @since 2.0.0.0
reinterpretH :: forall e1 e2 r a
                . EffHandlerH e1 r (e2 ': r)
               -> Sem (e1 ': r) a
               -> Sem (e2 ': r) a
reinterpretH = genericInterpretH
{-# INLINE reinterpretH #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- @since 0.1.0.0
reinterpret :: forall e1 e2 r a
              . FirstOrder e1 "reinterpret"
             => (∀ z x. e1 z x -> Sem (e2 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': r) a
reinterpret h =
  reinterpretH $ \e -> raise (h e)
{-# INLINE[2] reinterpret #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret2', but for higher-order effects. See 'interpretH' for a
-- simple usage guide.
--
-- @since 2.0.0.0
reinterpret2H :: forall e1 e2 e3 r a
                 . EffHandlerH e1 r (e2 ': e3 ': r)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': r) a
reinterpret2H = genericInterpretH
{-# INLINE[2] reinterpret2H #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /two/ intermediary effects.
--
-- @since 0.1.0.0
reinterpret2 :: forall e1 e2 e3 r a
              . FirstOrder e1 "reinterpret2"
             => (∀ z x. e1 z x -> Sem (e2 ': e3 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': e3 ': r) a
reinterpret2 h =
  reinterpret2H $ \e -> raise (h e)
{-# INLINE[2] reinterpret2 #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret3', but for higher-order effects. See 'interpretH' for a
-- simple usage guide.
--
-- @since TODO
reinterpret3H :: forall e1 e2 e3 e4 r a
                 . EffHandlerH e1 r (e2 ': e3 ': e4 ': r)
                -> Sem (e1 ': r) a
                -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3H = genericInterpretH
{-# INLINE reinterpret3H #-}

------------------------------------------------------------------------------
-- | Like 'reinterpret', but introduces /three/ intermediary effects.
--
-- @since TODO
reinterpret3 :: forall e1 e2 e3 e4 r a
              . FirstOrder e1 "reinterpret3"
             => (∀ z x. e1 z x -> Sem (e2 ': e3 ': e4 ': r) x)
             -> Sem (e1 ': r) a
             -> Sem (e2 ': e3 ': e4 ': r) a
reinterpret3 h =
  reinterpret3H $ \e -> raise (h e)
{-# INLINE[2] reinterpret3 #-}

------------------------------------------------------------------------------
-- | Like 'intercept', but for higher-order effects.
--
-- @since TODO
intercept :: forall e r a
              . Member e r
             => EffHandlerH e r r
             -> Sem r a
             -> Sem r a
intercept h = interpretH h . expose
{-# INLINE intercept #-}

------------------------------------------------------------------------------
-- | Like 'interpret', but instead of handling the effect, allows responding to
-- the effect while leaving it unhandled. This allows you, for example, to
-- intercept other effects and insert logic around them.
--
-- See 'interpretH' for a simple usage guide.
--
-- @since 2.0.0.0
interceptH :: forall e r a
              . FirstOrder e "intercept"
             => Member e r
             => (∀ z x. e z x -> Sem r x)
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
                  -> EffHandlerH e r r
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
                  -> (∀ z x. e z x -> Sem r x)
                  -> Sem r a
                  -> Sem r a
interceptUsingH pr h =
  interceptUsing pr $ \e -> raise (h e)
{-# INLINE interceptUsingH #-}
