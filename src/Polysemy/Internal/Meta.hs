{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The meta-effect 'Meta'
module Polysemy.Internal.Meta where

import Data.Kind (Type)

import Data.Unique
import Data.Type.Equality
import Polysemy
import Polysemy.Membership
import Polysemy.Internal.Union

type MetaEffect = [(Type -> Type, Effect)] -> Effect

data MetaRun :: Effect where
  MetaRun :: forall eff m a. Unique -> eff m a -> MetaRun m a

type (:%) a b = '(a, b)

data Meta (metaeff :: MetaEffect) :: Effect where
  MetaMetaRun :: forall metaeff m a
               . MetaRun m a -> Meta metaeff m a
  SendMeta :: forall metaeff l z m a
            . metaeff l z a
           -> (forall x. z x -> m x)
           -> (forall eff q x. Unique -> ElemOf '(q, eff) l -> q x -> m x)
           -> Meta metaeff m a

class AllSemRaises l r where
  proveSemRaise :: forall eff z. ElemOf '(z, eff) l -> z :~: Sem (eff ': r)

instance AllSemRaises '[] r where
  proveSemRaise = absurdMembership

instance (t ~ '(Sem (eff ': r), eff), AllSemRaises l' r)
      => AllSemRaises (t ': l') r where
  proveSemRaise Here = Refl
  proveSemRaise (There pr) = proveSemRaise pr

sendMeta :: forall metaeff l r a
          . (Member (Meta metaeff) r, AllSemRaises l r)
         => metaeff l (Sem r) a
         -> Sem r a
sendMeta = sendMetaUsing membership

sendMetaUsing :: forall metaeff l r a
               . AllSemRaises l r
              => ElemOf (Meta metaeff) r
              -> metaeff l (Sem r) a
              -> Sem r a
sendMetaUsing pr = sendMetaViaUsing pr
                    id
                    (\pr' -> case proveSemRaise @l @r pr' of Refl -> id)

sendMetaVia :: forall metaeff l m r a
             . Member (Meta metaeff) r
            => (forall x. m x -> Sem r x)
            -> (forall eff z x. ElemOf '(z, eff) l -> z x -> Sem (eff ': r) x)
            -> metaeff l m a
            -> Sem r a
sendMetaVia = sendMetaViaUsing membership

sendMetaViaUsing :: forall metaeff l m r a
                  . ElemOf (Meta metaeff) r
                 -> (forall x. m x -> Sem r x)
                 -> (forall eff z x. ElemOf '(z, eff) l -> z x -> Sem (eff ': r) x)
                 -> metaeff l m a
                 -> Sem r a
sendMetaViaUsing pr to1 to2 p = sendUsing pr $ SendMeta p to1 $ \uniq pr' ->
  transformUsing pr (MetaMetaRun @metaeff . MetaRun uniq) . to2 pr'

metaToMeta ::
  ∀ metaeff0 metaeff1 r.
  Member (Meta metaeff1) r =>
  (forall l m x. metaeff0 l m x -> metaeff1 l m x) ->
  InterpreterFor (Meta metaeff0) r
metaToMeta = metaToMetaUsing membership

metaToMetaUsing ::
  ∀ metaeff0 metaeff1 r.
  ElemOf (Meta metaeff1) r ->
  (forall l m x. metaeff0 l m x -> metaeff1 l m x) ->
  InterpreterFor (Meta metaeff0) r
metaToMetaUsing pr n = transformUsing pr \case
  MetaMetaRun metarun -> MetaMetaRun metarun
  SendMeta metaeff to1 to2 -> SendMeta (n metaeff) to1 to2

metaIntoMeta ::
  ∀ metaeff0 metaeff1 r.
  (forall l m x. metaeff0 l m x -> metaeff1 l m x) ->
  (forall x. Sem (Meta metaeff0 ': r) x -> Sem (Meta metaeff1 ': r) x)
metaIntoMeta n = rewrite \case
  MetaMetaRun metarun -> MetaMetaRun metarun
  SendMeta metaeff to1 to2 -> SendMeta (n metaeff) to1 to2

mkIntoMeta ::
  ∀ eff metaeff m x.
  (eff m x -> metaeff '[] m x) ->
  eff m x -> Meta metaeff m x
mkIntoMeta n e = SendMeta (n e) id (\_ u _ -> absurdMembership u)
