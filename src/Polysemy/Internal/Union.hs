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
  , WeavingDetails (..)
  , Member
  , MemberWithError
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
  , ElemOf (..)
  , membership
  , sameMember
  -- * Checking membership
  , KnownRow
  , tryMembership
  ) where

import Control.Monad
import Data.Functor.Compose
import Data.Functor.Identity
import Data.Kind
import Data.Typeable
import Polysemy.Internal.Kind
import {-# SOURCE #-} Polysemy.Internal

#ifndef NO_ERROR_MESSAGES
import Polysemy.Internal.CustomErrors
#endif


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
  fmap f (Union w t) = Union w $ fmap f t
  {-# INLINE fmap #-}


data Weaving e mAfter resultType where
  Weaving
    :: forall f e rBefore a resultType mAfter. (Functor f)
    => !(WeavingDetails f e rBefore a resultType mAfter)
    -> Weaving e mAfter resultType

data WeavingDetails f e rBefore a resultType mAfter =
  WeavingDetails {
    weaveEffect :: e (Sem rBefore) a
    -- ^ The original effect GADT originally lifted via
    -- 'Polysemy.Internal.send'.
  , weaveState :: f ()
    -- ^ A piece of state that other effects' interpreters have already
    -- woven through this 'Weaving'. @f@ is a 'Functor', so you can always
    -- 'fmap' into this thing.
  , weaveDistrib :: forall x. f (Sem rBefore x) -> mAfter (f x)
    -- ^ Distribute @f@ by transforming @Sem rBefore@ into @mAfter@. We have an
    -- invariants on @rBefore@, which means in actuality this function looks like
    -- @f ('Polysemy.Sem' rBefore x) -> mAfter (f x)@.
  , weaveResult :: f a -> resultType
    -- ^ Even though @f a@ is the moral resulting type of 'Weaving', we
    -- can't expose that fact; such a thing would prevent 'Polysemy.Sem'
    -- from being a 'Monad'.
  , weaveInspect :: forall x. f x -> Maybe x
    -- ^ A function for attempting to see inside an @f@. This is no
    -- guarantees that such a thing will succeed (for example,
    -- 'Polysemy.Error.Error' might have 'Polysemy.Error.throw'n.)
  }

instance Functor (Weaving e m) where
  fmap f (Weaving (WeavingDetails e s d f' v)) =
    Weaving $ WeavingDetails e s d (f . f') v
  {-# INLINE fmap #-}



weave
    :: (Functor s, Functor m, Functor n)
    => s ()
    -> (∀ x. s (m x) -> n (s x))
    -> (∀ x. s x -> Maybe x)
    -> Union r m a
    -> Union r n (s a)
weave s' d v' (Union w (Weaving (WeavingDetails e s nt f v))) =
  Union w $ Weaving $ WeavingDetails
              e (Compose $ s <$ s')
              (fmap Compose . d . fmap nt . getCompose)
              (fmap f . getCompose)
              (v <=< v' . getCompose)
{-# INLINE weave #-}


hoist
    :: (∀ x. m x -> n x)
    -> Union r m a
    -> Union r n a
hoist f' (Union w (Weaving (WeavingDetails e s nt f v))) =
  Union w $ Weaving $ WeavingDetails e s (f' . nt) f v
{-# INLINE hoist #-}


------------------------------------------------------------------------------
-- | A proof that the effect @e@ is available somewhere inside of the effect
-- stack @r@.
type Member e r = MemberNoError e r

------------------------------------------------------------------------------
-- | Like 'Member', but will produce an error message if the types are
-- ambiguous. This is the constraint used for actions generated by
-- 'Polysemy.makeSem'.
--
-- /Be careful with this./ Due to quirks of 'GHC.TypeLits.TypeError',
-- the custom error messages emitted by this can potentially override other,
-- more helpful error messages.
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
extract (Union (There g) _) = case g of {}
{-# INLINE extract #-}


------------------------------------------------------------------------------
-- | An empty union contains nothing, so this function is uncallable.
absurdU :: Union '[] m a -> b
absurdU (Union pr _) = case pr of {}


------------------------------------------------------------------------------
-- | Weaken a 'Union' so it is capable of storing a new sort of effect.
weaken :: forall e r m a. Union r m a -> Union (e ': r) m a
weaken (Union pr a) = Union (There pr) a
{-# INLINE weaken #-}



------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it.
inj :: forall e r rInitial a. (Member e r) => e (Sem rInitial) a -> Union r (Sem rInitial) a
inj e = injWeaving $
  Weaving $ WeavingDetails e (Identity ())
              (fmap Identity . runIdentity)
              runIdentity
              (Just . runIdentity)
{-# INLINE inj #-}


------------------------------------------------------------------------------
-- | Lift an effect @e@ into a 'Union' capable of holding it,
-- given an explicit proof that the effect exists in @r@
injUsing :: forall e r rInitial a.
  ElemOf e r -> e (Sem rInitial) a -> Union r (Sem rInitial) a
injUsing pr e = Union pr $ Weaving $ WeavingDetails
            e (Identity ())
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
