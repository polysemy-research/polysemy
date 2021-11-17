{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE StrictData              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Union
  ( Union (..)
  , Weaving (..)
  , Member
  , MemberWithError
  , weave
  , hoist
  , liftHandler
  , liftHandlerWithNat

  -- * Building Unions
  , inj
  , injUsing
  , injWeaving
  , mkWeaving
  , weaken

  -- * Using Unions
  , decomp
  , prj
  , prjUsing
  , extract
  , absurdU
  , decompCoerce
  -- * Witnesses
  , ElemOf (..)
  , membership
  , sameMember
  -- * Checking membership
  , KnownRow
  , tryMembership
  , extendMembershipLeft
  , extendMembershipRight
  , injectMembership
  , weakenList
  , weakenMid

  , module Polysemy.Internal.WeaveClass

  ) where

import Control.Monad.Trans.Identity
import Data.Coerce
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Typeable
import Polysemy.Internal.Kind
import Polysemy.Internal.WeaveClass
import {-# SOURCE #-} Polysemy.Internal
import Polysemy.Internal.Sing (SList (SEnd, SCons))

#ifndef NO_ERROR_MESSAGES
import Polysemy.Internal.CustomErrors
#endif

-- liftHandler :: (forall x. e m x -> n x)
--             -> (forall x. e (t m) x -> t n x)

class Weavable e where
  weave :: (Functor s, Monad m, Monad n)
        => s ()
        -> (forall x. s (m x) -> n (s x))
        -> e m a -> e n (s a)

liftHandlerStateT :: (Weavable e, Monad m, Monad n)
                  => (forall x. e m x -> n x)
                  -> (forall x. e (StateT s m) x -> StateT s n x)
liftHandlerStateT h = \(eff :: e (StateT s m) a) ->
  StateT $ \initS ->
    let
      initState :: (s, ())
      initState =  (initS, ())

      distribute :: forall x
                  . (s, StateT exc m x)
                 -> m (s, x)
      distribute (s, st) = swap <$> runStateT st s

      weavedEff :: e m (s, a)
      weavedEff = weave initState distribute eff
    in
      h weavedEff

liftHandler :: ( Weavable e, MonadTransWeave t
               , Monad m, Monad n
               )
            => (forall x. e m x -> n x)
            -> e (t m) a -> t n a
liftHandler alg e = controlT $ \lower -> do
    initSt <- lower (pure ())
    alg $ weave
            initSt
            (lower . join . restoreT)
            e

data Weaving e m a where
  Weaving :: Functor s
          => s ()
          -> (forall x. s (z x) -> m (s x))
          -> (s a -> b)
          -> e z a
          -> Weaving e m b
{-


newtype Sem r a = Sem { runSem :: ∀ m . Monad m => (∀ x. Union r (Sem r) x -> m x) -> m a }
newtype Sem r a = Sem { runSem :: ∀ m . Monad m => (∀ e x. ElemOf e r -> Weaving e (Sem r) x -> m x) -> m a }
newtype Sem r a = Sem { runSem :: ∀ m . Monad m => (∀ x. Weaving e0 (Sem r) x -> m x)
                                                -> (∀ x. Weaving e1 (Sem r) x -> m x)
                                                -> ...
                                                -> m a }


  forall x. Weaving e (Sem r) x -> m x
~ forall s z x q. Functor s => s () -> (forall y. s (z y) -> Sem r (s y)) -> e z q -> (s q -> x) -> m x
~ forall s z q. Functor s => s () -> (forall y. s (z y) -> Sem r (s y)) -> e z q -> m (s q)
~ forall s z q. Functor s => (forall y. s (z y) -> Sem r (s y)) -> e z x -> s () -> m (s x)
is VERY ROUGHLY equivalent to
~ forall t x. MonadTransControl t => (forall y. z y -> t (Sem r) y) -> e z x -> (forall z y. t z x -> z (StT t y)) -> m (StT t x)
Roughly equivalent to
~ forall t x. MonadTransControl t => (forall y. z y -> t (Sem r) y) -> e z x -> t m x


In the context of a 'interpret' use:
forall t x. MonadTransControl t => (forall y. z y -> t (Sem (e ': r)) y) -> e z x -> t (Sem r) x
Sem (e ': r) (t y)

liftSend :: (Member e r, MonadTransControl t) => e (t (Sem r)) a -> t (Sem r) a
liftSend = liftWeavingHandler (liftSem . injWeaving) . mkWeaving

-- See the discussion in
-- <https://github.com/polysemy-research/polysemy/issues/227 Issue #227>.
--
-- @since 1.2.3.0
type MemberWithError e r =
  ( MemberNoError e r
#ifndef NO_ERROR_MESSAGES
    -- NOTE: The plugin explicitly pattern matches on
    -- `WhenStuck (LocateEffect _ r) _`, so if you change this, make sure to change
    -- the corresponding implementation in
    -- Polysemy.Plugin.Fundep.solveBogusError
  , WhenStuck (LocateEffect e r) (AmbiguousSend e r)
#endif
  )

type MemberNoError e r =
  ( Find e r
#ifndef NO_ERROR_MESSAGES
  , LocateEffect e r ~ '()
#endif
  )

------------------------------------------------------------------------------
-- | A proof that @e@ is an element of @r@.
--
-- Due to technical reasons, @'ElemOf' e r@ is not powerful enough to
-- prove @'Member' e r@; however, it can still be used send actions of @e@
-- into @r@ by using 'Polysemy.Internal.subsumeUsing'.
--
-- @since 1.3.0.0
data ElemOf e r where
  -- | @e@ is located at the head of the list.
  Here  :: ElemOf e (e ': r)
  -- | @e@ is located somewhere in the tail of the list.
  There :: ElemOf e r -> ElemOf e (e' ': r)

------------------------------------------------------------------------------
-- | Checks if two membership proofs are equal. If they are, then that means
-- that the effects for which membership is proven must also be equal.
sameMember :: forall e e' r
            . ElemOf e r
           -> ElemOf e' r
           -> Maybe (e :~: e')
sameMember Here       Here =
  Just Refl
sameMember (There pr) (There pr') =
  sameMember @e @e' pr pr'
sameMember (There _)  _ =
  Nothing
sameMember _          _ =
  Nothing


------------------------------------------------------------------------------
-- | Used to detect ambiguous uses of effects. If @r@ isn't concrete,
-- and we haven't been given @'LocateEffect' e r ~ '()@ from a
-- @'Member' e r@ constraint, then @'LocateEffect' e r@ will get stuck.
type family LocateEffect (t :: k) (ts :: [k]) :: () where
#ifndef NO_ERROR_MESSAGES
  LocateEffect t '[] = UnhandledEffect t
#endif
  LocateEffect t (t ': ts) = '()
  LocateEffect t (u ': ts) = LocateEffect t ts

class Find (t :: k) (r :: [k]) where
  membership' :: ElemOf t r

instance {-# OVERLAPPING #-} Find t (t ': z) where
  membership' = Here
  {-# INLINE membership' #-}

instance Find t z => Find t (_1 ': z) where
  membership' = There $ membership' @_ @t @z
  {-# INLINE membership' #-}

------------------------------------------------------------------------------
-- | A class for effect rows whose elements are inspectable.
--
-- This constraint is eventually satisfied as @r@ is instantied to a
-- monomorphic list.
-- (E.g when @r@ becomes something like
-- @'['Polysemy.State.State' Int, 'Polysemy.Output.Output' String, 'Polysemy.Embed' IO]@)
class KnownRow r where
  tryMembership' :: forall e. Typeable e => Maybe (ElemOf e r)

instance KnownRow '[] where
  tryMembership' = Nothing
  {-# INLINE tryMembership' #-}

instance (Typeable e, KnownRow r) => KnownRow (e ': r) where
  tryMembership' :: forall e'. Typeable e' => Maybe (ElemOf e' (e ': r))
  tryMembership' = case eqT @e @e' of
    Just Refl -> Just Here
    _         -> There <$> tryMembership' @r @e'
  {-# INLINE tryMembership' #-}

------------------------------------------------------------------------------
-- | Given @'Member' e r@, extract a proof that @e@ is an element of @r@.
membership :: Member e r => ElemOf e r
membership = membership'
{-# INLINE membership #-}

------------------------------------------------------------------------------
-- | Extracts a proof that @e@ is an element of @r@ if that
-- is indeed the case; otherwise returns @Nothing@.
tryMembership :: forall e r. (Typeable e, KnownRow r) => Maybe (ElemOf e r)
tryMembership = tryMembership' @r @e
{-# INLINE tryMembership #-}


------------------------------------------------------------------------------
-- | Extends a proof that @e@ is an element of @r@ to a proof that @e@ is an
-- element of the concatenation of the lists @l@ and @r@.
-- @l@ must be specified as a singleton list proof.
extendMembershipLeft :: forall l r e. SList l -> ElemOf e r -> ElemOf e (Append l r)
extendMembershipLeft SEnd pr = pr
extendMembershipLeft (SCons l) pr = There (extendMembershipLeft l pr)
{-# INLINE extendMembershipLeft #-}


------------------------------------------------------------------------------
-- | Extends a proof that @e@ is an element of @l@ to a proof that @e@ is an
-- element of the concatenation of the lists @l@ and @r@.
extendMembershipRight :: forall l r e. ElemOf e l -> ElemOf e (Append l r)
extendMembershipRight Here = Here
extendMembershipRight (There e) = There (extendMembershipRight @_ @r e)
{-# INLINE extendMembershipRight #-}


------------------------------------------------------------------------------
-- | Extends a proof that @e@ is an element of @left <> right@ to a proof that
-- @e@ is an element of @left <> mid <> right@.
-- Both @left@ and @right@ must be specified as singleton list proofs.
injectMembership :: forall right e left mid
                  . SList left
                 -> SList mid
                 -> ElemOf e (Append left right)
                 -> ElemOf e (Append left (Append mid right))
injectMembership SEnd sm pr = extendMembershipLeft sm pr
injectMembership (SCons _) _ Here = Here
injectMembership (SCons sl) sm (There pr) = There (injectMembership @right sl sm pr)
{-# INLINE injectMembership #-}


------------------------------------------------------------------------------
-- | Decompose a 'Union'. Either this union contains an effect @e@---the head
-- of the @r@ list---or it doesn't.
decomp :: Union (e ': r) m a -> Either (Union r m a) (Weaving e m a)
decomp (Union p a) =
  case p of
    Here  -> Right a
    There pr -> Left $ Union pr a
{-# INLINE decomp #-}

------------------------------------------------------------------------------
-- | Retrieve the last effect in a 'Union'.
extract :: Union '[e] m a -> Weaving e m a
extract (Union Here a)   = a
#if __GLASGOW_HASKELL__ < 808
extract (Union (There g) _) = case g of {}
#endif
{-# INLINE extract #-}


------------------------------------------------------------------------------
-- | An empty union contains nothing, so this function is uncallable.
absurdU :: Union '[] m a -> b
#if __GLASGOW_HASKELL__ < 808
absurdU (Union pr _) = case pr of {}
#else
absurdU = \case {}
#endif


------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a new sort of effect at the
-- head.
weaken :: forall e r m a. Union r m a -> Union (e ': r) m a
weaken (Union pr a) = Union (There pr) a
{-# INLINE weaken #-}


------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a number of new effects at
-- the head, specified as a singleton list proof.
weakenList :: SList l -> Union r m a -> Union (Append l r) m a
weakenList sl (Union pr e) = Union (extendMembershipLeft sl pr) e
{-# INLINE weakenList #-}


------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a number of new effects
-- somewhere within the previous effect list.
-- Both the prefix and the new effects are specified as singleton list proofs.
weakenMid :: forall right m a left mid
           . SList left -> SList mid
          -> Union (Append left right) m a
          -> Union (Append left (Append mid right)) m a
weakenMid sl sm (Union pr e) = Union (injectMembership @right sl sm pr) e
{-# INLINE weakenMid #-}


------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it.
inj :: forall e r rInitial a. Member e r => e (Sem rInitial) a -> Union r (Sem rInitial) a
inj = injWeaving . mkWeaving
{-# INLINE inj #-}


mkWeaving :: forall e rInitial a. e (Sem rInitial) a -> Weaving e (Sem rInitial) a
mkWeaving e = Weaving
  e
  (\ nt -> coerce nt)
  (fmap Identity . runIdentityT)
  runIdentity
{-# INLINE mkWeaving #-}


------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it,
-- given an explicit proof that the effect exists in @r@
injUsing :: forall e r rInitial a.
  ElemOf e r -> e (Sem rInitial) a -> Union r (Sem rInitial) a
injUsing pr e = Union pr $ Weaving
  e
  (\ nt -> coerce nt)
  (fmap Identity . runIdentityT)
  runIdentity
{-# INLINE injUsing #-}

------------------------------------------------------------------------------
-- | Lift a @'Weaving' e@ into a 'Union' capable of holding it.
injWeaving :: forall e r m a. Member e r => Weaving e m a -> Union r m a
injWeaving = Union membership
{-# INLINE injWeaving #-}

------------------------------------------------------------------------------
-- | Attempt to take an @e@ effect out of a 'Union'.
prj :: forall e r m a
     . ( Member e r
       )
    => Union r m a
    -> Maybe (Weaving e m a)
prj = prjUsing membership
{-# INLINE prj #-}

------------------------------------------------------------------------------
-- | Attempt to take an @e@ effect out of a 'Union', given an explicit
-- proof that the effect exists in @r@.
prjUsing
  :: forall e r m a
   . ElemOf e r
  -> Union r m a
  -> Maybe (Weaving e m a)
prjUsing pr (Union sn a) = (\Refl -> a) <$> sameMember pr sn
{-# INLINE prjUsing #-}

------------------------------------------------------------------------------
-- | Like 'decomp', but allows for a more efficient
-- 'Polysemy.Interpretation.reinterpret' function.
decompCoerce
    :: Union (e ': r) m a
    -> Either (Union (f ': r) m a) (Weaving e m a)
decompCoerce (Union p a) =
  case p of
    Here  -> Right a
    There pr -> Left (Union (There pr) a)
{-# INLINE decompCoerce #-}
