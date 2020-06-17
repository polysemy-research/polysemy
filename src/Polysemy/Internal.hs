{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE MonoLocalBinds       #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal
  ( Sem (..)
  , Member
  , MemberWithError
  , Members
  , send
  , sendUsing
  , embed
  , run
  , runM
  , raise
  , raiseUnder
  , raiseUnder2
  , raiseUnder3
  , subsume
  , subsumeUsing
  , Embed (..)
  , usingSem
  , liftSem
  , hoistSem
  , InterpreterFor
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
import Polysemy.Embed.Type
import Polysemy.Fail.Type
import Polysemy.Internal.Fixpoint
import Polysemy.Internal.NonDet
import Polysemy.Internal.PluginLookup
import Polysemy.Internal.Union


-- $setup
-- >>> import Data.Function
-- >>> import Polysemy.State
-- >>> import Polysemy.Error

------------------------------------------------------------------------------
-- | The 'Sem' monad handles computations of arbitrary extensible effects.
-- A value of type @Sem r@ describes a program with the capabilities of
-- @r@. For best results, @r@ should always be kept polymorphic, but you can
-- add capabilities via the 'Member' constraint.
--
-- The value of the 'Sem' monad is that it allows you to write programs
-- against a set of effects without a predefined meaning, and provide that
-- meaning later. For example, unlike with mtl, you can decide to interpret an
-- 'Polysemy.Error.Error' effect traditionally as an 'Either', or instead
-- as (a significantly faster) 'IO' 'Control.Exception.Exception'. These
-- interpretations (and others that you might add) may be used interchangeably
-- without needing to write any newtypes or 'Monad' instances. The only
-- change needed to swap interpretations is to change a call from
-- 'Polysemy.Error.runError' to 'Polysemy.Error.errorToIOFinal'.
--
-- The effect stack @r@ can contain arbitrary other monads inside of it. These
-- monads are lifted into effects via the 'Embed' effect. Monadic values can be
-- lifted into a 'Sem' via 'embed'.
--
-- Higher-order actions of another monad can be lifted into higher-order actions
-- of 'Sem' via the 'Polysemy.Final' effect, which is more powerful
-- than 'Embed', but also less flexible to interpret.
--
-- A 'Sem' can be interpreted as a pure value (via 'run') or as any
-- traditional 'Monad' (via 'runM' or 'Polysemy.runFinal').
-- Each effect @E@ comes equipped with some interpreters of the form:
--
-- @
-- runE :: 'Sem' (E ': r) a -> 'Sem' r a
-- @
--
-- which is responsible for removing the effect @E@ from the effect stack. It
-- is the order in which you call the interpreters that determines the
-- monomorphic representation of the @r@ parameter.
--
-- Order of interpreters can be important - it determines behaviour of effects
-- that manipulate state or change control flow. For example, when
-- interpreting this action:
--
-- >>> :{
--   example :: Members '[State String, Error String] r => Sem r String
--   example = do
--     put "start"
--     let throwing, catching :: Members '[State String, Error String] r => Sem r String
--         throwing = do
--           modify (++"-throw")
--           throw "error"
--           get
--         catching = do
--           modify (++"-catch")
--           get
--     catch @String throwing (\ _ -> catching)
-- :}
--
-- when handling 'Polysemy.Error.Error' first, state is preserved after error
-- occurs:
--
-- >>> :{
--   example
--     & runError
--     & fmap (either id id)
--     & evalState ""
--     & runM
--     & (print =<<)
-- :}
-- "start-throw-catch"
--
-- while handling 'Polysemy.State.State' first discards state in such cases:
--
-- >>> :{
--   example
--     & evalState ""
--     & runError
--     & fmap (either id id)
--     & runM
--     & (print =<<)
-- :}
-- "start-catch"
--
-- A good rule of thumb is to handle effects which should have \"global\"
-- behaviour over other effects later in the chain.
--
-- After all of your effects are handled, you'll be left with either
-- a @'Sem' '[] a@, a @'Sem' '[ 'Embed' m ] a@, or a @'Sem' '[ 'Polysemy.Final' m ] a@
-- value, which can be consumed respectively by 'run', 'runM', and
-- 'Polysemy.runFinal'.
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
  a <|> b = send (Choose a b)
  {-# INLINE (<|>) #-}

-- | @since 0.2.1.0
instance (Member NonDet r) => MonadPlus (Sem r) where
  mzero = empty
  mplus = (<|>)

-- | @since 1.1.0.0
instance (Member Fail r) => MonadFail (Sem r) where
  fail = send . Fail
  {-# INLINE fail #-}


------------------------------------------------------------------------------
-- | This instance will only lift 'IO' actions. If you want to lift into some
-- other 'MonadIO' type, use this instance, and handle it via the
-- 'Polysemy.IO.embedToMonadIO' interpretation.
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
--
-- 'raiseUnder' can be used in order to turn transformative interpreters
-- into reinterpreters. This is especially useful if you're writing an interpreter
-- which introduces an intermediary effect, and then want to use an existing
-- interpreter on that effect.
--
-- For example, given:
--
-- @
-- fooToBar :: 'Member' Bar r => 'Sem' (Foo ': r) a -> 'Sem' r a
-- runBar   :: 'Sem' (Bar ': r) a -> 'Sem' r a
-- @
--
-- You can write:
--
-- @
-- runFoo :: 'Sem' (Foo ': r) a -> 'Sem' r a
-- runFoo =
--     runBar     -- Consume Bar
--   . fooToBar   -- Interpret Foo in terms of the new Bar
--   . 'raiseUnder' -- Introduces Bar under Foo
-- @
--
-- @since 1.2.0.0
raiseUnder :: ∀ e2 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': r) a
raiseUnder = hoistSem $ hoist raiseUnder . weakenUnder
  where
    weakenUnder :: ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': r) m x
    weakenUnder (Union Here a) = Union Here a
    weakenUnder (Union (There n) a) = Union (There (There n)) a
    {-# INLINE weakenUnder #-}
{-# INLINE raiseUnder #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces two new effects underneath the head of the
-- list.
--
-- @since 1.2.0.0
raiseUnder2 :: ∀ e2 e3 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': r) a
raiseUnder2 = hoistSem $ hoist raiseUnder2 . weakenUnder2
  where
    weakenUnder2 ::  ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': e3 ': r) m x
    weakenUnder2 (Union Here a) = Union Here a
    weakenUnder2 (Union (There n) a) = Union (There (There (There n))) a
    {-# INLINE weakenUnder2 #-}
{-# INLINE raiseUnder2 #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces three new effects underneath the head of the
-- list.
--
-- @since 1.2.0.0
raiseUnder3 :: ∀ e2 e3 e4 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': e4 ': r) a
raiseUnder3 = hoistSem $ hoist raiseUnder3 . weakenUnder3
  where
    weakenUnder3 ::  ∀ m x. Union (e1 ': r) m x -> Union (e1 ': e2 ': e3 ': e4 ': r) m x
    weakenUnder3 (Union Here a) = Union Here a
    weakenUnder3 (Union (There n) a) = Union (There (There (There (There n)))) a
    {-# INLINE weakenUnder3 #-}
{-# INLINE raiseUnder3 #-}

------------------------------------------------------------------------------
-- | Interprets an effect in terms of another identical effect.
--
-- This is useful for defining interpreters that use 'Polysemy.reinterpretH'
-- without immediately consuming the newly introduced effect.
-- Using such an interpreter recursively may result in duplicate effects,
-- which may then be eliminated using 'subsume'.
--
-- @since 1.2.0.0
subsume :: Member e r => Sem (e ': r) a -> Sem r a
subsume = subsumeUsing membership
{-# INLINE subsume #-}

------------------------------------------------------------------------------
-- | Interprets an effect in terms of another identical effect, given an
-- explicit proof that the effect exists in @r@.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership'
-- in order to conditionally make use of effects. For example:
--
-- @
-- tryListen :: 'Polysemy.Membership.KnownRow' r => 'Sem' r a -> Maybe ('Sem' r ([Int], a))
-- tryListen m = case 'Polysemy.Membership.tryMembership' @('Polysemy.Writer.Writer' [Int]) of
--   Just pr -> Just $ 'subsumeUsing' pr ('Polysemy.Writer.listen' ('raise' m))
--   _       -> Nothing
-- @
--
-- @since 1.3.0.0
subsumeUsing :: forall e r a. ElemOf e r -> Sem (e ': r) a -> Sem r a
subsumeUsing pr =
  let
    go :: forall x. Sem (e ': r) x -> Sem r x
    go = hoistSem $ \u -> hoist go $ case decomp u of
      Right w -> Union pr w
      Left  g -> g
    {-# INLINE go #-}
  in
    go
{-# INLINE subsumeUsing #-}


------------------------------------------------------------------------------
-- | Embed an effect into a 'Sem'. This is used primarily via
-- 'Polysemy.makeSem' to implement smart constructors.
send :: Member e r => e (Sem r) a -> Sem r a
send = liftSem . inj
{-# INLINE[3] send #-}

------------------------------------------------------------------------------
-- | Embed an effect into a 'Sem', given an explicit proof
-- that the effect exists in @r@.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership',
-- in order to conditionally make use of effects.
sendUsing :: ElemOf e r -> e (Sem r) a -> Sem r a
sendUsing pr = liftSem . injUsing pr
{-# INLINE[3] sendUsing #-}


------------------------------------------------------------------------------
-- | Embed a monadic action @m@ in 'Sem'.
--
-- @since 1.0.0.0
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
-- | Type synonym for interpreters that consume an effect without changing the
-- return value. Offered for user convenience.
--
-- @r@ Is kept polymorphic so it's possible to place constraints upon it:
--
-- @
-- teletypeToIO :: 'Member' (Embed IO) r
--              => 'InterpreterFor' Teletype r
-- @
type InterpreterFor e r = forall a. Sem (e ': r) a -> Sem r a

------------------------------------------------------------------------------
-- | Some interpreters need to be able to lower down to the base monad (often
-- 'IO') in order to function properly --- some good examples of this are
-- 'Polysemy.Error.lowerError' and 'Polysemy.Resource.lowerResource'.
--
-- However, these interpreters don't compose particularly nicely; for example,
-- to run 'Polysemy.Resource.lowerResource', you must write:
--
-- @
-- runM . lowerError runM
-- @
--
-- Notice that 'runM' is duplicated in two places here. The situation gets
-- exponentially worse the more intepreters you have that need to run in this
-- pattern.
--
-- Instead, '.@' performs the composition we'd like. The above can be written as
--
-- @
-- (runM .@ lowerError)
-- @
--
-- The parentheses here are important; without them you'll run into operator
-- precedence errors.
--
-- __Warning:__ This combinator will __duplicate work__ that is intended to be
-- just for initialization. This can result in rather surprising behavior. For
-- a version of '.@' that won't duplicate work, see the @.\@!@ operator in
-- <http://hackage.haskell.org/package/polysemy-zoo/docs/Polysemy-IdempotentLowering.html polysemy-zoo>.
--
-- Interpreters using 'Polysemy.Final' may be composed normally, and
-- avoid the work duplication issue. For that reason, you're encouraged to use
-- @-'Polysemy.Final'@ interpreters instead of @lower-@ interpreters whenever
-- possible.
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
-- 'Polysemy.Error.lowerError'.
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

