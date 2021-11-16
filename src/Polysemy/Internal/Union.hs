{-# LANGUAGE AllowAmbiguousTypes     #-}
{-# LANGUAGE CPP                     #-}
{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE EmptyCase               #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE FunctionalDependencies  #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE PatternSynonyms         #-}
{-# LANGUAGE RoleAnnotations         #-}
{-# LANGUAGE StrictData              #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE ViewPatterns            #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Union
  ( Union (..)
  , Weaving (..)
  , Member
  , weave
  , hoist
  -- * Building Unions
  , inj
  , injUsing
  , injWeaving
  , weaken
  -- * Using Unions
  , decomp
  , prj
  , prjUsing
  , extract
  , absurdU
  , decompCoerce
  -- * Witnesses
  , ElemOf (Here, There)
  , membership
  , sameMember
  -- * Checking membership
  , KnownRow
  , tryMembership
  , extendMembershipLeft
  , extendMembershipRight
  , injectMembership
  , weakenList
  , weakenMid) where

import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Typeable
import Polysemy.Internal.Kind
import {-# SOURCE #-} Polysemy.Internal
import Polysemy.Internal.Sing (SList (SEnd, SCons))
import Unsafe.Coerce (unsafeCoerce)


------------------------------------------------------------------------------
-- | An extensible, type-safe union. The @r@ type parameter is a type-level
-- list of effects, any one of which may be held within the 'Union'.
data Union (r :: EffectRow) (mWoven :: Type -> Type) a where
  Union
      :: -- A proof that the effect is actually in @r@.
         ElemOf e r
         -- The effect to wrap. The functions 'prj' and 'decomp' can help
         -- retrieve this value later.
      -> Weaving e m a
      -> Union r m a

instance Functor (Union r mWoven) where
  fmap f (Union w t) = Union w $ f <$> t
  {-# INLINE fmap #-}


data Weaving e mAfter resultType where
  Weaving
    :: forall f e rInitial a resultType mAfter. (Functor f)
    => {
      weaveEffect :: e (Sem rInitial) a
      -- ^ The original effect GADT originally lifted via
      -- 'Polysemy.Internal.send'.
      -- ^ @rInitial@ is the effect row that was in scope when this 'Weaving'
      -- was originally created.
      , weaveState :: f ()
      -- ^ A piece of state that other effects' interpreters have already
      -- woven through this 'Weaving'. @f@ is a 'Functor', so you can always
      -- 'fmap' into this thing.
      , weaveDistrib :: forall x. f (Sem rInitial x) -> mAfter (f x)
      -- ^ Distribute @f@ by transforming @Sem rInitial@ into @mAfter@. This is
      -- usually of the form @f ('Polysemy.Sem' (Some ': Effects ': r) x) ->
      --   Sem r (f x)@
      , weaveResult :: f a -> resultType
      -- ^ Even though @f a@ is the moral resulting type of 'Weaving', we
      -- can't expose that fact; such a thing would prevent 'Polysemy.Sem'
      -- from being a 'Monad'.
      , weaveInspect :: forall x. f x -> Maybe x
      -- ^ A function for attempting to see inside an @f@. This is no
      -- guarantees that such a thing will succeed (for example,
      -- 'Polysemy.Error.Error' might have 'Polysemy.Error.throw'n.)
      } -> Weaving e mAfter resultType

instance Functor (Weaving e m) where
  fmap f (Weaving e s d f' v) = Weaving e s d (f . f') v
  {-# INLINE fmap #-}



weave
    :: (Functor s, Functor n)
    => s ()
    -> (∀ x. s (m x) -> n (s x))
    -> (∀ x. s x -> Maybe x)
    -> Union r m a
    -> Union r n (s a)
weave s' d v' (Union w (Weaving e s nt f v)) =
  Union w $ Weaving
              e (Compose $ s <$ s')
              (fmap Compose . d . fmap nt . getCompose)
              (fmap f . getCompose)
              (v <=< v' . getCompose)
{-# INLINE weave #-}


hoist
    :: (∀ x. m x -> n x)
    -> Union r m a
    -> Union r n a
hoist f' (Union w (Weaving e s nt f v)) =
  Union w $ Weaving e s (f' . nt) f v
{-# INLINE hoist #-}

------------------------------------------------------------------------------
-- | A proof that @e@ is an element of @r@.
--
-- Due to technical reasons, @'ElemOf' e r@ is not powerful enough to
-- prove @'Member' e r@; however, it can still be used send actions of @e@
-- into @r@ by using 'Polysemy.Internal.subsumeUsing'.
--
-- @since 1.3.0.0
type role ElemOf nominal nominal
newtype ElemOf (e :: k) (r :: [k]) = UnsafeMkElemOf Int

data MatchHere e r where
  MHYes :: MatchHere e (e ': r)
  MHNo  :: MatchHere e r

data MatchThere e r where
  MTYes :: ElemOf e r -> MatchThere e (e' ': r)
  MTNo  :: MatchThere e r

matchHere :: forall e r. ElemOf e r -> MatchHere e r
matchHere (UnsafeMkElemOf 0) = unsafeCoerce $ MHYes
matchHere _ = MHNo

matchThere :: forall e r. ElemOf e r -> MatchThere e r
matchThere (UnsafeMkElemOf 0) = MTNo
matchThere (UnsafeMkElemOf e) = unsafeCoerce $ MTYes $ UnsafeMkElemOf $ e - 1

pattern Here :: () => (r ~ (e ': r')) => ElemOf e r
pattern Here <- (matchHere -> MHYes)
  where
    Here = UnsafeMkElemOf 0

pattern There :: () => (r' ~ (e' ': r)) => ElemOf e r -> ElemOf e r'
pattern There e <- (matchThere -> MTYes e)
  where
    There (UnsafeMkElemOf e) = UnsafeMkElemOf $ e + 1

{-# COMPLETE Here, There #-}

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


class Member (t :: Effect) (r :: EffectRow) where
  membership' :: ElemOf t r

instance {-# OVERLAPPING #-} Member t (t ': z) where
  membership' = Here
  {-# INLINE membership' #-}

instance Member t z => Member t (_1 ': z) where
  membership' = There $ membership' @t @z
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
extract (Union (There _) _) = error "Unsafe use of UnsafeMkElemOf"
{-# INLINE extract #-}


------------------------------------------------------------------------------
-- | An empty union contains nothing, so this function is uncallable.
absurdU :: Union '[] m a -> b
absurdU (Union _ _) = error "Unsafe use of UnsafeMkElemOf"


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
inj :: forall e r rInitial a. (Member e r) => e (Sem rInitial) a -> Union r (Sem rInitial) a
inj e = injWeaving $ Weaving
  e
  (Identity ())
  (fmap Identity . runIdentity)
  runIdentity
  (Just . runIdentity)
{-# INLINE inj #-}


------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it,
-- given an explicit proof that the effect exists in @r@
injUsing :: forall e r rInitial a.
  ElemOf e r -> e (Sem rInitial) a -> Union r (Sem rInitial) a
injUsing pr e = Union pr $ Weaving
  e
  (Identity ())
  (fmap Identity . runIdentity)
  runIdentity
  (Just . runIdentity)
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
