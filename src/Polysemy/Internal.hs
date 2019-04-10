{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

module Polysemy.Internal
  ( Semantic (..)
  , Member
  , send
  , sendM
  , run
  , runM
  , raise
  , Lift ()
  , usingSemantic
  , liftSemantic
  , hoistSemantic
  , (.@)
  , (.@@)
  ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Polysemy.Internal.Effect
import Polysemy.Internal.Fixpoint
import Polysemy.Internal.Lift
import Polysemy.Internal.NonDet
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | The 'Semantic' monad handles computations of arbitrary extensible effects.
-- A value of type @Semantic r@ describes a program with the capabilities of
-- @r@. For best results, @r@ should always be kept polymorphic, but you can
-- add capabilities via the 'Member' constraint.
--
-- The value of the 'Semantic' monad is that it allows you to write programs
-- against a set of effects without a predefined meaning, and provide that
-- meaning later. For example, unlike with mtl, you can decide to interpret an
-- 'Polysemy.Error.Error' effect tradtionally as an 'Either', or instead
-- significantly faster as an 'IO' 'Control.Exception.Exception'. These
-- interpretations (and others that you might add) may be used interchangably
-- without needing to write any newtypes or 'Monad' instances. The only
-- change needed to swap interpretations is to change a call from
-- 'Polysemy.Error.runError' to 'Polysemy.Error.runErrorInIO'.
--
-- The effect stack @r@ can contain arbitrary other monads inside of it. These
-- monads are lifted into effects via the 'Lift' effect. Monadic values can be
-- lifted into a 'Semantic' via 'sendM'.
--
-- A 'Semantic' can be interpreted as a pure value (via 'run') or as any
-- traditional 'Monad' (via 'runM'). Each effect @E@ comes equipped with some
-- interpreters of the form:
--
-- @
-- runE :: 'Semantic' (E ': r) a -> 'Semantic' r a
-- @
--
-- which is responsible for removing the effect @E@ from the effect stack. It
-- is the order in which you call the interpreters that determines the
-- monomorphic representation of the @r@ parameter.
--
-- After all of your effects are handled, you'll be left with either
-- a @'Semantic' '[] a@ or a @'Semantic' ('Lift' m) a@ value, which can be
-- consumed respectively by 'run' and 'runM'.
--
-- ==== Examples
--
-- As an example of keeping @r@ polymorphic, we can consider the type
--
-- @
-- 'Member' ('Polysemy.State' String) r => 'Semantic' r ()
-- @
--
-- to be a program with access to
--
-- @
-- 'Polysemy.State.get' :: 'Semantic' r String
-- 'Polysemy.State.put' :: String -> 'Semantic' r ()
-- @
--
-- methods.
--
-- By also adding a
--
-- @
-- 'Member' ('Polysemy.Error' Bool) r
-- @
--
-- constraint on @r@, we gain access to the
--
-- @
-- 'Polysemy.Error.throw' :: Bool -> 'Semantic' r a
-- 'Polysemy.Error.catch' :: 'Semantic' r a -> (Bool -> 'Semantic' r a) -> 'Semantic' r a
-- @
--
-- functions as well.
--
-- In this sense, a @'Member' ('Polysemy.State.State' s) r@ constraint is
-- analogous to mtl's @'Control.Monad.State.Class.MonadState' s m@ and should
-- be thought of as such. However, /unlike/ mtl, a 'Semantic' monad may have
-- an arbitrary number of the same effect.
--
-- For example, we can write a 'Semantic' program which can output either
-- 'Int's or 'Bool's:
--
-- @
-- foo :: ( 'Member' ('Polysemy.Output.Output' Int) r
--        , 'Member' ('Polysemy.Output.Output' Bool) r
--        )
--     => 'Semantic' r ()
-- foo = do
--   'Polysemy.Output.output' @Int  5
--   'Polysemy.Output.output' True
-- @
--
-- Notice that we must use @-XTypeApplications@ to specify that we'd like to
-- use the ('Polysemy.Output.Output' 'Int') effect.
newtype Semantic r a = Semantic
  { runSemantic
        :: ∀ m
         . Monad m
        => (∀ x. Union r (Semantic r) x -> m x)
        -> m a
  }

------------------------------------------------------------------------------
-- | Like 'runSemantic' but flipped for better ergonomics sometimes.
usingSemantic
    :: Monad m
    => (∀ x. Union r (Semantic r) x -> m x)
    -> Semantic r a
    -> m a
usingSemantic k m = runSemantic m k
{-# INLINE usingSemantic #-}


instance Functor (Semantic f) where
  fmap f (Semantic m) = Semantic $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Semantic f) where
  pure a = Semantic $ const $ pure a
  {-# INLINE pure #-}

  Semantic f <*> Semantic a = Semantic $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Semantic f) where
  return = pure
  {-# INLINE return #-}

  Semantic ma >>= f = Semantic $ \k -> do
    z <- ma k
    runSemantic (f z) k
  {-# INLINE (>>=) #-}


instance (Member NonDet r) => Alternative (Semantic r) where
  empty = send Empty
  a <|> b = do
    send (Choose id) >>= \case
      False -> a
      True  -> b


instance (Member (Lift IO) r) => MonadIO (Semantic r) where
  liftIO = sendM
  {-# INLINE liftIO #-}

instance Member Fixpoint r => MonadFix (Semantic r) where
  mfix f = send $ Fixpoint f


liftSemantic :: Union r (Semantic r) a -> Semantic r a
liftSemantic u = Semantic $ \k -> k u
{-# INLINE liftSemantic #-}


hoistSemantic
    :: (∀ x. Union r (Semantic r) x -> Union r' (Semantic r') x)
    -> Semantic r a
    -> Semantic r' a
hoistSemantic nat (Semantic m) = Semantic $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistSemantic #-}


------------------------------------------------------------------------------
-- | Introduce an effect into 'Semantic'. Analogous to
-- 'Control.Monad.Class.Trans.lift' in the mtl ecosystem
raise :: ∀ e r a. Semantic r a -> Semantic (e ': r) a
raise = hoistSemantic $ hoist raise_b . weaken
{-# INLINE raise #-}


raise_b :: Semantic r a -> Semantic (e ': r) a
raise_b = raise
{-# NOINLINE raise_b #-}


------------------------------------------------------------------------------
-- | Lift an effect into a 'Semantic'. This is used primarily via
-- 'Polysemy.makeSemantic' to implement smart constructors.
send :: Member e r => e (Semantic r) a -> Semantic r a
send = liftSemantic . inj
{-# INLINE[3] send #-}


------------------------------------------------------------------------------
-- | Lift a monadic action @m@ into 'Semantic'.
sendM :: Member (Lift m) r => m a -> Semantic r a
sendM = send . Lift
{-# INLINE sendM #-}


------------------------------------------------------------------------------
-- | Run a 'Semantic' containing no effects as a pure value.
run :: Semantic '[] a -> a
run (Semantic m) = runIdentity $ m absurdU
{-# INLINE run #-}


------------------------------------------------------------------------------
-- | Lower a 'Semantic' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Semantic '[Lift m] a -> m a
runM (Semantic m) = m $ \z ->
  case extract z of
    Yo e s _ f -> do
      a <- unLift e
      pure $ f $ a <$ s
{-# INLINE runM #-}


------------------------------------------------------------------------------
-- | Some interpreters need to be able to lower down to the base monad (often
-- 'IO') in order to function properly --- some good examples of this are
-- 'Polysemy.Error.runErrorInIO' and 'Polysemy.Resource.runResource'.
--
-- However, these interpreters don't compose particularly nicely; for example,
-- to run 'Polysemy.Resource.runResource', you must write:
--
-- @
-- runM . runErrorInIO runM
-- @
--
-- Notice that 'runM' is duplicated in two places here. The situation gets
-- exponentially worse the more intepreters you have that need to run in this
-- pattern.
--
-- Instead, '.@' performs the composition we'd like. The above can be written as
--
-- @
-- (runM .@ runErrorInIO)
-- @
--
-- The parentheses here are important; without them you'll run into operator
-- precedence errors.
(.@)
    :: Monad m
    => (∀ x. Semantic r x -> m x)
       -- ^ The lowering function, likely 'runM'.
    -> (∀ y. (∀ x. Semantic r x -> m x)
          -> Semantic (e ': r) y
          -> Semantic r y)
    -> Semantic (e ': r) z
    -> m z
f .@ g = f . g f
infixl 9 .@


------------------------------------------------------------------------------
-- | Like '.@', but for interpreters which change the resulting type --- eg.
-- 'Polysemy.Error.runErrorInIO'.
(.@@)
    :: Monad m
    => (∀ x. Semantic r x -> m x)
       -- ^ The lowering function, likely 'runM'.
    -> (∀ y. (∀ x. Semantic r x -> m x)
          -> Semantic (e ': r) y
          -> Semantic r (f y))
    -> Semantic (e ': r) z
    -> m (f z)
f .@@ g = f . g f
infixl 9 .@@

