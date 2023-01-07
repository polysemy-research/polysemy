{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MonoLocalBinds        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The 'Sem' type and the most basic stack manipulation utilities
module Polysemy.Internal
  ( Sem (..)
  , Member
  , Members
  , send
  , sendVia
  , sendUsing
  , sendViaUsing
  , embed
  , run
  , raise_
  , Raise (..)
  , raise
  , raiseUnder
  , raiseUnder2
  , raiseUnder3
  , raise2Under
  , raise3Under
  , subsume_
  , Subsume (..)
  , subsume
  , subsumeUsing
  , insertAt
  , sinkBelow
  , floatAbove
  , expose
  , exposeUsing
  , Embed (..)
  , usingSem
  , liftSem
  , hoistSem
  , mapMembership
  , Append
  , InterpreterFor
  , InterpretersFor
  ) where

import Control.Applicative
import Control.Monad
#if __GLASGOW_HASKELL__ < 808


import Control.Monad.Fail
#endif
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Functor.Identity
import Data.Kind
import Data.Type.Equality
import Polysemy.Embed.Type
import Polysemy.Fail.Type
import Polysemy.Internal.Fixpoint
import Polysemy.Internal.Index (InsertAtIndex(insertAtIndex))
import Polysemy.Internal.Kind
import Polysemy.Internal.NonDet
import Polysemy.Internal.PluginLookup
import Polysemy.Internal.Union
import Polysemy.Internal.Sing


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
-- traditional 'Monad' (via 'Polysemy.runM').
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
  fmap f (Sem m) = Sem $ \k -> f <$> m k
  -- {-# INLINE fmap #-}


instance Applicative (Sem f) where
  pure a = Sem $ \_ -> pure a
  -- {-# INLINE pure #-}

  Sem f <*> Sem a = Sem $ \k -> f k <*> a k
  -- {-# INLINE (<*>) #-}

  liftA2 f ma mb = Sem $ \k -> liftA2 f (runSem ma k) (runSem mb k)
  -- {-# INLINE liftA2 #-}

  ma <* mb = Sem $ \k -> runSem ma k <* runSem mb k
  -- {-# INLINE (<*) #-}

  -- Use (>>=) because many monads are bad at optimizing (*>).
  -- Ref https://github.com/polysemy-research/polysemy/issues/368
  ma *> mb = Sem $ \k -> runSem ma k >>= \_ -> runSem mb k
  -- {-# INLINE (*>) #-}

instance Monad (Sem f) where
  Sem ma >>= f = Sem $ \k -> do
    z <- ma k
    runSem (f z) k
  -- {-# INLINE (>>=) #-}


instance (Member NonDet r) => Alternative (Sem r) where
  empty = send Empty
  -- {-# INLINE empty #-}
  a <|> b = send (Choose a b)
  -- {-# INLINE (<|>) #-}

-- | @since 0.2.1.0
instance (Member NonDet r) => MonadPlus (Sem r) where
  mzero = empty
  mplus = (<|>)

-- | @since 1.1.0.0
instance (Member Fail r) => MonadFail (Sem r) where
  fail = send . Fail
  -- {-# INLINE fail #-}

-- | @since 1.6.0.0
instance Semigroup a => Semigroup (Sem f a) where
  (<>) = liftA2 (<>)

-- | @since 1.6.0.0
instance Monoid a => Monoid (Sem f a) where
  mempty = pure mempty

------------------------------------------------------------------------------
-- | This instance will only lift 'IO' actions. If you want to lift into some
-- other 'MonadIO' type, use this instance, and handle it via the
-- 'Polysemy.IO.embedToMonadIO' interpretation.
instance Member (Embed IO) r => MonadIO (Sem r) where
  liftIO = embed
  {-# INLINE liftIO #-}

instance Member Fixpoint r => MonadFix (Sem r) where
  mfix f = send $ Fixpoint f
  -- {-# INLINE mfix #-}


------------------------------------------------------------------------------
-- | Create a 'Sem' from a 'Union' with matching stacks.
liftSem :: Union r (Sem r) a -> Sem r a
liftSem u = Sem $ \k -> k u
{-# INLINE liftSem #-}

------------------------------------------------------------------------------
-- | Extend the stack of a 'Sem' with an explicit 'Union' transformation.
hoistSem
    :: (∀ x. Union r (Sem r) x -> Union r' (Sem r') x)
    -> Sem r a
    -> Sem r' a
hoistSem nat (Sem m) = Sem $ \k -> m $ \u -> k $ nat u
{-# INLINE hoistSem #-}

------------------------------------------------------------------------------
-- | Rewrite the effect stack of a 'Sem' using with an explicit membership proof
-- transformation.
mapMembership :: forall r r' a
               . (forall e. ElemOf e r -> ElemOf e r')
              -> Sem r a
              -> Sem r' a
mapMembership n = go
  where
    go :: forall x. Sem r x -> Sem r' x
    go = hoistSem $ \(Union pr wav) -> hoist go_ $ Union (n pr) wav
    {-# INLINE go #-}

    go_ :: forall x. Sem r x -> Sem r' x
    go_ = go
    {-# NOINLINE go_ #-}
{-# NOINLINE[0] mapMembership #-}

{-# RULES
"mapMembership/id" [2]
  forall m.
    mapMembership idMembership m = m

"mapMembership/mapMembership" [1]
  forall (f :: forall e. ElemOf e r' -> ElemOf e r'')
         (g :: forall e. ElemOf e r -> ElemOf e r') m.
    mapMembership f (mapMembership g m) = mapMembership (f . g) m
#-}

------------------------------------------------------------------------------
-- | Introduce an arbitrary number of effects on top of the effect stack. This
-- function is highly polymorphic, so it may be good idea to use its more
-- concrete versions (like 'raise') or type annotations to avoid vague errors
-- in ambiguous contexts.
--
-- @since 1.4.0.0
raise_ :: ∀ r r' a. Raise r r' => Sem r a -> Sem r' a
raise_ = mapMembership raiseMembership
{-# INLINE raise_ #-}


-- | See 'raise'.
--
-- @since 1.4.0.0
class Raise (r :: EffectRow) (r' :: EffectRow) where
  raiseMembership :: ElemOf e r -> ElemOf e r'

instance {-# INCOHERENT #-} Raise r r where
  raiseMembership = idMembership
  {-# INLINE raiseMembership #-}

instance {-# OVERLAPPING #-} Raise (e ': r) (e ': r) where
  raiseMembership = idMembership
  {-# INLINE raiseMembership #-}

instance Raise r r' => Raise r (_0 ': r') where
  raiseMembership = There . raiseMembership

------------------------------------------------------------------------------
-- | Introduce an effect into 'Sem'. Analogous to
-- 'Control.Monad.Class.Trans.lift' in the mtl ecosystem. For a variant that
-- can introduce an arbitrary number of effects, see 'raise_'.
raise :: ∀ e r a. Sem r a -> Sem (e ': r) a
raise = raise_
{-# INLINE raise #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces a new effect underneath the head of the
-- list. See 'raiseUnder2' or 'raiseUnder3' for introducing more effects. If
-- you need to introduce even more of them, check out 'subsume_'.
--
-- 'raiseUnder' can be used in order to turn transformative interpreters
-- into reinterpreters. This is especially useful if you're writing an
-- interpreter which introduces an intermediary effect, and then want to use
-- an existing interpreter on that effect.
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
raiseUnder = subsume_
{-# INLINE raiseUnder #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces two new effects underneath the head of the
-- list.
--
-- @since 2.0.0.0
raise2Under :: ∀ e2 e3 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': r) a
raise2Under = subsume_
{-# INLINE raise2Under #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces three new effects underneath the head of the
-- list.
--
-- @since 2.0.0.0
raise3Under :: ∀ e2 e3 e4 e1 r a. Sem (e1 ': r) a -> Sem (e1 ': e2 ': e3 ': e4 ': r) a
raise3Under = subsume_
{-# INLINE raise3Under #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces an effect two levels underneath the head of
-- the list.
--
-- @since 2.0.0.0
raiseUnder2 :: ∀ e3 e1 e2 r a. Sem (e1 : e2 : r) a -> Sem (e1 : e2 : e3 : r) a
raiseUnder2 = subsume_
{-# INLINE raiseUnder2 #-}


------------------------------------------------------------------------------
-- | Like 'raise', but introduces an effect three levels underneath the head
-- of the list.
--
-- @since 2.0.0.0
raiseUnder3 :: ∀ e4 e1 e2 e3 r a. Sem (e1 : e2 : e3 : r) a -> Sem (e1 : e2 : e3 : e4 : r) a
raiseUnder3 = subsume_
{-# INLINE raiseUnder3 #-}


------------------------------------------------------------------------------
-- | Allows reordering and adding known effects on top of the effect stack, as
-- long as the polymorphic "tail" of new stack is a 'raise'-d version of the
-- original one. This function is highly polymorphic, so it may be a good idea
-- to use its more concrete version ('subsume'), fitting functions from the
-- 'raise' family or type annotations to avoid vague errors in ambiguous
-- contexts.
--
-- @since 1.4.0.0
subsume_ :: ∀ r r' a. Subsume r r' => Sem r a -> Sem r' a
subsume_ = mapMembership subsumeMembership
{-# INLINE subsume_ #-}

class Raise' (r :: EffectRow) (r' :: EffectRow) where
  raiseMembership' :: ElemOf e r -> ElemOf e r'

instance {-# INCOHERENT #-} Raise' r r where
  raiseMembership' = idMembership
  {-# INLINE raiseMembership' #-}

instance {-# OVERLAPPING #-} Raise' (e ': r) (e ': r) where
  raiseMembership' = idMembership
  {-# INLINE raiseMembership' #-}

instance Subsume r r' => Raise' r (_0 ': r') where
  raiseMembership' = There . subsumeMembership

class Subsume' (origR' :: EffectRow) (r :: EffectRow) (r' :: EffectRow) where
  subsumeMembership' :: (ElemOf e r' -> ElemOf e origR')
                     -> ElemOf e r -> ElemOf e origR'

instance Subsume r origR' => Subsume' origR' r r' where
  subsumeMembership' _ = subsumeMembership
  {-# INLINE subsumeMembership' #-}

instance {-# INCOHERENT #-} Subsume' origR r r'
  => Subsume' origR (e ': r) (e ': r') where
  subsumeMembership' t Here = t Here
  subsumeMembership' t (There pr) = subsumeMembership' (t . There) pr

instance {-# INCOHERENT #-} Subsume' origR (e ': r) (e ': r) where
  subsumeMembership' t = t
  {-# INLINE subsumeMembership' #-}

-- | See 'subsume_'.
--
-- @since 1.4.0.0
class Subsume (r :: EffectRow) (r' :: EffectRow) where
  subsumeMembership :: ElemOf e r -> ElemOf e r'

instance {-# INCOHERENT #-} Raise' r r' => Subsume r r' where
  subsumeMembership = raiseMembership'
  {-# INLINE subsumeMembership #-}

instance {-# INCOHERENT #-} Subsume' (e ': r') (e ': r) (e ': r')
      => Subsume (e ': r) (e ': r') where
  subsumeMembership = subsumeMembership' idMembership
  {-# INLINE subsumeMembership #-}

instance Subsume '[] r where
  subsumeMembership = absurdMembership
  {-# INLINE subsumeMembership #-}

instance (Member e r', Subsume r r') => Subsume (e ': r) r' where
  subsumeMembership Here = membership
  subsumeMembership (There pr) = subsumeMembership pr

------------------------------------------------------------------------------
-- | Interprets an effect in terms of another identical effect.
--
-- This is useful for defining interpreters that use 'Polysemy.reinterpretH'
-- without immediately consuming the newly introduced effect.
-- Using such an interpreter recursively may result in duplicate effects,
-- which may then be eliminated using 'subsume'.
--
-- For a version that can introduce an arbitrary number of new effects and
-- reorder existing ones, see 'subsume_'.
--
-- @since 1.2.0.0
subsume :: ∀ e r a. Member e r => Sem (e ': r) a -> Sem r a
subsume = subsume_
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
-- 'subsumeUsing' is also useful when you encounter issues with
-- 'Polysemy.Member', as the membership proof can be used to explicitly target
-- a specific effect.
--
-- @
-- localUnder :: forall i e r a. 'Polysemy.Member' ('Polysemy.Reader.Reader' i) r
--            => (i -> i) -> 'Sem' (e ': r) a -> 'Sem' (e ': r) a
-- localUnder f m = 'Polysemy.Membership.subsumeUsing' @(Reader i) ('Polysemy.Membership.There' 'Polysemy.Membership.membership') ('Polysemy.Reader.local' f ('Polysemy.raise' m))
-- @
--
-- @since 1.3.0.0
subsumeUsing :: ∀ e r a. ElemOf e r -> Sem (e ': r) a -> Sem r a
subsumeUsing pr = mapMembership \case
  Here -> pr
  There pr' -> pr'
{-# INLINE subsumeUsing #-}

------------------------------------------------------------------------------
-- | Moves all uses of an effect @e@ within the argument computation
-- to a new @e@ placed on top of the effect stack. Note that this does not
-- consume the inner @e@.
--
-- This can be used to create interceptors out of interpreters.
-- For example:
--
-- @
-- 'Polysemy.intercept' k = 'Polysemy.interpret' k . 'expose'
-- @
--
-- @since TODO
expose :: Member e r => Sem r a -> Sem (e ': r) a
expose = exposeUsing membership
{-# INLINE expose #-}

------------------------------------------------------------------------------
-- | Introduce a set of effects into 'Sem' at the index @i@, before the effect
-- that previously occupied that position. This is intended to be used with a
-- type application:
--
-- @
-- let
--   sem1 :: Sem [e1, e2, e3, e4, e5] a
--   sem1 = insertAt @2 (sem0 :: Sem [e1, e2, e5] a)
-- @
--
-- @since 1.6.0.0
insertAt
  :: forall index inserted head oldTail tail old full a
   . ( ListOfLength "insertAt" index head
     , old ~ Append head oldTail
     , tail ~ Append inserted oldTail
     , full ~ Append head tail
     , InsertAtIndex index head tail oldTail full inserted)
  => Sem old a
  -> Sem full a
insertAt = mapMembership $
  injectMembership
    @oldTail
    (listOfLength @"insertAt" @index @head)
    (insertAtIndex @Effect @index @head @tail @oldTail @full @inserted)
{-# INLINE insertAt #-}

-- | Given an explicit proof that @e@ exists in @r@, moves all uses of e@
-- within the argument computation to a new @e@ placed on top of the effect
-- stack. Note that this does not consume the inner @e@.
--
-- This is useful in conjunction with 'Polysemy.Internal.Union.tryMembership'
-- and 'interpret'\/'interpretH' in order to conditionally perform
-- 'intercept'-like operations.
--
-- @since 2.0.0.0
exposeUsing :: forall e r a. ElemOf e r -> Sem r a -> Sem (e ': r) a
exposeUsing pr = mapMembership \pr' ->
  case sameMember pr pr' of
    Just Refl -> Here
    _         -> There pr'
{-# INLINE exposeUsing #-}

------------------------------------------------------------------------------
-- | Execute an action of an effect.
--
-- This is primarily used to create methods for actions of effects:
--
-- @
-- data FooBar m a where
--   Foo :: String -> m a -> FooBar m a
--   Bar :: FooBar m Int
--
-- foo :: Member FooBar r => String -> Sem r a -> Sem r a
-- foo s m = send (Foo s m)
--
-- bar :: Member FooBar r => Sem r Int
-- bar = send Bar
-- @
--
-- 'Polysemy.makeSem' allows you to eliminate this boilerplate.
send :: Member e r => e (Sem r) a -> Sem r a
send = liftSem . inj
{-# NOINLINE[3] send #-}

------------------------------------------------------------------------------
-- | Execute an action of an effect, given a natural transformation from
-- the monad used for the higher-order chunks in the effect to
-- @'Polysemy.Sem' r@.
--
-- @since 2.0.0.0
sendVia :: forall e z r a
         . Member e r
        => (forall x. z x -> Sem r x)
        -> e z a -> Sem r a
sendVia n = liftSem . hoist n . inj
{-# NOINLINE[3] sendVia #-}

------------------------------------------------------------------------------
-- | Embed an effect into a 'Sem', given an explicit proof
-- that the effect exists in @r@.
--
-- This is useful in conjunction with 'Polysemy.Membership.tryMembership',
-- in order to conditionally make use of effects.
sendUsing :: ElemOf e r -> e (Sem r) a -> Sem r a
sendUsing pr = liftSem . injUsing pr
{-# NOINLINE[3] sendUsing #-}

------------------------------------------------------------------------------
-- | Embed an effect into a 'Sem', given an explicit proof
-- that the effect exists in @r@, and a natural transformation from the monad
-- used for the higher-order thunks in the effect to @'Polysemy.Sem' r@.
sendViaUsing :: ElemOf e r -> (forall x. z x -> Sem r x) -> e z a -> Sem r a
sendViaUsing pr n = liftSem . hoist n . injUsing pr
{-# NOINLINE[3] sendViaUsing #-}


------------------------------------------------------------------------------
-- | Embed a monadic action @m@ in 'Sem'.
--
-- @since 1.0.0.0
embed :: Member (Embed m) r => m a -> Sem r a
embed = send . Embed


------------------------------------------------------------------------------
-- | Run a 'Sem' containing no effects as a pure value.
run :: Sem '[] a -> a
run (Sem m) = runIdentity $ m absurdU
{-# INLINE run #-}


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
type InterpreterFor e r = ∀ a. Sem (e ': r) a -> Sem r a


------------------------------------------------------------------------------
-- | Variant of 'InterpreterFor' that takes a list of effects.
-- @since 1.5.0.0
type InterpretersFor es r = ∀ a. Sem (Append es r) a -> Sem r a


sinkBelow :: forall l e r a
           . KnownList l => Sem (e ': Append l r) a -> Sem (Append l (e ': r)) a
sinkBelow = mapMembership \case
  Here -> extendMembershipLeft @(e ': r) (singList @l) Here
  There pr -> injectMembership @r (singList @l) (singList @'[e]) pr

floatAbove :: forall l e r a
           . KnownList l => Sem (Append l (e ': r)) a -> Sem (e ': Append l r) a
floatAbove = mapMembership \pr ->
  case splitMembership @(e ': r) (singList @l) pr of
    Left pr' -> There (extendMembershipRight @_ @r pr')
    Right Here -> Here
    Right (There pr') ->
      There (extendMembershipLeft @r (singList @l) pr')
