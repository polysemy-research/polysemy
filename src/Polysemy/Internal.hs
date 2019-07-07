{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal
  ( Sem (..)
  , Member
  , Members
  , send
  , embed
  , run
  , runM
  , raise
  , raiseUnder
  , raiseUnder2
  , raiseUnder3
  , Embed (..)
  , usingSem
  , liftSem
  , hoistSem
  , (.@)
  , (.@@)
  ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Kind
import Polysemy.Internal.Fixpoint
import Polysemy.Embed.Type
import Polysemy.Internal.NonDet
import Polysemy.Internal.PluginLookup
import Polysemy.Internal.Union


------------------------------------------------------------------------------
-- | The 'Sem' monad handles computations of arbitrary extensible effects.
-- A value of type @Sem r@ describes a program with the capabilities of
-- @r@. For best results, @r@ should always be kept polymorphic, but you can
-- add capabilities via the 'Member' constraint.
--
-- The value of the 'Sem' monad is that it allows you to write programs
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
-- monads are lifted into effects via the 'Embed' effect. Monadic values can be
-- lifted into a 'Sem' via 'embed'.
--
-- A 'Sem' can be interpreted as a pure value (via 'run') or as any
-- traditional 'Monad' (via 'runM'). Each effect @E@ comes equipped with some
-- interpreters of the form:
--
-- @
-- runE :: 'Sem' (E ': r) a -> 'Sem' r a
-- @
--
-- which is responsible for removing the effect @E@ from the effect stack. It
-- is the order in which you call the interpreters that determines the
-- monomorphic representation of the @r@ parameter.
--
-- After all of your effects are handled, you'll be left with either
-- a @'Sem' '[] a@ or a @'Sem' '[ 'Embed' m ] a@ value, which can be
-- consumed respectively by 'run' and 'runM'.
--
-- ==== Examples
--
-- As an example of keeping @r@ polymorphic, we can consider the type
--
-- @
-- 'Member' ('Polysemy.State.State' String) r => 'Sem' r ()
-- @
--
-- to be a program with access to
--
-- @
-- 'Polysemy.State.get' :: 'Sem' r String
-- 'Polysemy.State.put' :: String -> 'Sem' r ()
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
-- 'Polysemy.Error.throw' :: Bool -> 'Sem' r a
-- 'Polysemy.Error.catch' :: 'Sem' r a -> (Bool -> 'Sem' r a) -> 'Sem' r a
-- @
--
-- functions as well.
--
-- In this sense, a @'Member' ('Polysemy.State.State' s) r@ constraint is
-- analogous to mtl's @'Control.Monad.State.Class.MonadState' s m@ and should
-- be thought of as such. However, /unlike/ mtl, a 'Sem' monad may have
-- an arbitrary number of the same effect.
--
-- For example, we can write a 'Sem' program which can output either
-- 'Int's or 'Bool's:
--
-- @
-- foo :: ( 'Member' ('Polysemy.Output.Output' Int) r
--        , 'Member' ('Polysemy.Output.Output' Bool) r
--        )
--     => 'Sem' r ()
-- foo = do
--   'Polysemy.Output.output' @Int  5
--   'Polysemy.Output.output' True
-- @
--
-- Notice that we must use @-XTypeApplications@ to specify that we'd like to
-- use the ('Polysemy.Output.Output' 'Int') effect.
--
-- @since 0.1.2.0
newtype Sem r a = Sem
  { runSem
        :: ∀ m
         . Monad m
        => (∀ x. Union r (Sem r) x -> m x)
        -> m a
  }


------------------------------------------------------------------------------
-- | Due to a quirk of the GHC plugin interface, it's only easy to find
-- transitive dependencies if they define an orphan instance. This orphan
-- instance allows us to find "Polysemy.Internal" in the polysemy-plugin.
instance PluginLookup Plugin


------------------------------------------------------------------------------
-- | Makes constraints of functions that use multiple effects shorter by
-- translating single list of effects into multiple 'Member' constraints:
--
-- @
-- foo :: 'Members' \'[ 'Polysemy.Output.Output' Int
--                 , 'Polysemy.Output.Output' Bool
--                 , 'Polysemy.State' String
--                 ] r
--     => 'Sem' r ()
-- @
--
-- translates into:
--
-- @
-- foo :: ( 'Member' ('Polysemy.Output.Output' Int) r
--        , 'Member' ('Polysemy.Output.Output' Bool) r
--        , 'Member' ('Polysemy.State' String) r
--        )
--     => 'Sem' r ()
-- @
--
-- @since 0.1.2.0
type family Members es r :: Constraint where
  Members '[]       r = ()
  Members (e ': es) r = (Member e r, Members es r)


------------------------------------------------------------------------------
-- | Like 'runSem' but flipped for better ergonomics sometimes.
usingSem
    :: Monad m
    => (∀ x. Union r (Sem r) x -> m x)
    -> Sem r a
    -> m a
usingSem k m = runSem m k
{-# INLINE usingSem #-}


instance Functor (Sem f) where
  fmap f (Sem m) = Sem $ \k -> fmap f $ m k
  {-# INLINE fmap #-}


instance Applicative (Sem f) where
  pure a = Sem $ const $ pure a
  {-# INLINE pure #-}

  Sem f <*> Sem a = Sem $ \k -> f k <*> a k
  {-# INLINE (<*>) #-}


instance Monad (Sem f) where
  return = pure
  {-# INLINE return #-}

  Sem ma >>= f = Sem $ \k -> do
    z <- ma k
    runSem (f z) k
  {-# INLINE (>>=) #-}


instance (Member NonDet r) => Alternative (Sem r) where
  empty = send Empty
  {-# INLINE empty #-}
  a <|> b = do
    send (Choose id) >>= \case
      False -> a
      True  -> b
  {-# INLINE (<|>) #-}

-- | @since 0.2.1.0
instance (Member NonDet r) => MonadPlus (Sem r) where
  mzero = empty
  mplus = (<|>)

-- | @since 0.2.1.0
instance (Member NonDet r) => MonadFail (Sem r) where
  fail = const empty


------------------------------------------------------------------------------
-- | This instance will only lift 'IO' actions. If you want to lift into some
-- other 'MonadIO' type, use this instance, and handle it via the
-- 'Polysemy.IO.runIO' interpretation.
instance (Member (Embed IO) r) => MonadIO (Sem r) where
  liftIO = embed
  {-# INLINE liftIO #-}

instance Member Fixpoint r => MonadFix (Sem r) where
  mfix f = send $ Fixpoint f
  {-# INLINE mfix #-}


liftSem :: Union r (Sem r) a -> Sem r a
liftSem u = Sem $ \k -> k u
{-# INLINE liftSem #-}


hoistSem
    :: (∀ x. Union r (Sem r) x -> Union r' (Sem r') x)
    -> Sem r a
    -> Sem r' a
hoistSem nat (Sem m) = Sem $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistSem #-}

------------------------------------------------------------------------------
-- | Introduce an effect into 'Sem'. Analogous to
-- 'Control.Monad.Class.Trans.lift' in the mtl ecosystem
raise :: ∀ e r a. Sem r a -> Sem (e ': r) a
raise = hoistSem $ hoist raise . weaken
{-# INLINE raise #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces a new effect underneath the head of the
-- list.
raiseUnder :: ∀ e2 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': r) a
raiseUnder = hoistSem $ hoist raiseUnder . weakenUnder
  where
    weakenUnder :: ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': r) m x
    weakenUnder (Union SZ a) = Union SZ a
    weakenUnder (Union (SS n) a) = Union (SS (SS n)) a
    {-# INLINE weakenUnder #-}
{-# INLINE raiseUnder #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces two new effects underneath the head of the
-- list.
raiseUnder2 :: ∀ e2 e3 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': r) a
raiseUnder2 = hoistSem $ hoist raiseUnder2 . weakenUnder2
  where
    weakenUnder2 ::  ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': e3 ': r) m x
    weakenUnder2 (Union SZ a) = Union SZ a
    weakenUnder2 (Union (SS n) a) = Union (SS (SS (SS n))) a
    {-# INLINE weakenUnder2 #-}
{-# INLINE raiseUnder2 #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces two new effects underneath the head of the
-- list.
raiseUnder3 :: ∀ e2 e3 e4 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': e4 ': r) a
raiseUnder3 = hoistSem $ hoist raiseUnder3 . weakenUnder3
  where
    weakenUnder3 ::  ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': e3 ': e4 ': r) m x
    weakenUnder3 (Union SZ a) = Union SZ a
    weakenUnder3 (Union (SS n) a) = Union (SS (SS (SS (SS n)))) a
    {-# INLINE weakenUnder3 #-}
{-# INLINE raiseUnder3 #-}


------------------------------------------------------------------------------
-- | Embed an effect into a 'Sem'. This is used primarily via
-- 'Polysemy.makeSem' to implement smart constructors.
send :: Member e r => e (Sem r) a -> Sem r a
send = liftSem . inj
{-# INLINE[3] send #-}


------------------------------------------------------------------------------
-- | Embed a monadic action @m@ in 'Sem'.
embed :: Member (Embed m) r => m a -> Sem r a
embed = send . Embed
{-# INLINE embed #-}


------------------------------------------------------------------------------
-- | Run a 'Sem' containing no effects as a pure value.
run :: Sem '[] a -> a
run (Sem m) = runIdentity $ m absurdU
{-# INLINE run #-}


------------------------------------------------------------------------------
-- | Lower a 'Sem' containing only a single lifted 'Monad' into that
-- monad.
runM :: Monad m => Sem '[Embed m] a -> m a
runM (Sem m) = m $ \z ->
  case extract z of
    Weaving e s _ f _ -> do
      a <- unEmbed e
      pure $ f $ a <$ s
{-# INLINE runM #-}


------------------------------------------------------------------------------
-- | Some interpreters need to be able to lower down to the base monad (often
-- 'IO') in order to function properly --- some good examples of this are
-- 'Polysemy.Error.runErrorInIO' and 'Polysemy.Resource.runResourceInIO'.
--
-- However, these interpreters don't compose particularly nicely; for example,
-- to run 'Polysemy.Resource.runResourceInIO', you must write:
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
--
-- __Warning:__ This combinator will __duplicate work__ that is intended to be
-- just for initialization. This can result in rather surprising behavior. For
-- a version of '.@' that won't duplicate work, see the @.\@!@ operator in
-- <http://hackage.haskell.org/package/polysemy-zoo/docs/Polysemy-IdempotentLowering.html polysemy-zoo>.
(.@)
    :: Monad m
    => (∀ x. Sem r x -> m x)
       -- ^ The lowering function, likely 'runM'.
    -> (∀ y. (∀ x. Sem r x -> m x)
          -> Sem (e ': r) y
          -> Sem r y)
    -> Sem (e ': r) z
    -> m z
f .@ g = f . g f
infixl 8 .@


------------------------------------------------------------------------------
-- | Like '.@', but for interpreters which change the resulting type --- eg.
-- 'Polysemy.Error.runErrorInIO'.
(.@@)
    :: Monad m
    => (∀ x. Sem r x -> m x)
       -- ^ The lowering function, likely 'runM'.
    -> (∀ y. (∀ x. Sem r x -> m x)
          -> Sem (e ': r) y
          -> Sem r (f y))
    -> Sem (e ': r) z
    -> m (f z)
f .@@ g = f . g f
infixl 8 .@@

