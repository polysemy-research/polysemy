{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE QuantifiedConstraints, FlexibleInstances, MultiParamTypeClasses #-}
module Polysemy.Newtype (
  coerceEff,
  coerceEffAt,
  coerceEffs,

  coerceSend,
  coerceSendVia,
  coerceSendUsing,
  coerceSendViaUsing,

  subsumeCoerce,
  subsumeCoerceUsing,
  ) where

import Data.Coerce
import Polysemy.Internal
import Polysemy.Internal.Kind
import Polysemy.Internal.Union
import Polysemy.Internal.Utils
import Unsafe.Coerce

-- Because for some reason (forall x. Coercible (e m x) (e' m x))
-- doesn't imply (Coercible e e')
class (forall m x. Coercible (e m x) (e' m x))
   => CoercibleEff (e :: Effect) (e' :: Effect)
instance (forall m x. Coercible (e m x) (e' m x)) => CoercibleEff e e'

-- TODO: swap type parameter order?
coerceEff :: forall e' e r a
           . CoercibleEff e e' => Sem (e ': r) a -> Sem (e' ': r) a
coerceEff = unsafeCoerce
{-# INLINE coerceEff #-}

-- Use type applications to specify the length of the prefix whenever necessary
--
-- @
-- coerceEffAt \@'[_, _]
-- @
--
-- TODO: use ListOfLength? Downside is that then you are required to specify
-- the length; upside is that's done through a simple number.
coerceEffAt :: forall l e' e r a
             . CoercibleEff e e'
            => Sem (Append l (e ': r)) a
            -> Sem (Append l (e' ': r)) a
coerceEffAt = unsafeCoerce
{-# INLINE coerceEffAt #-}

-- Here the specific type param order is very useful.
coerceSend :: forall e' e r a
            . (CoercibleEff e e', Member e' r)
           => e (Sem r) a -> Sem r a
coerceSend = coerceSendUsing @e' membership
{-# INLINE coerceSend #-}

coerceSendUsing :: forall e' e r a
                 . CoercibleEff e e'
                => ElemOf e' r
                -> e (Sem r) a -> Sem r a
coerceSendUsing pr = coerceSendViaUsing pr id
{-# INLINE coerceSendUsing #-}

coerceSendVia :: forall e' e z r a
               . (CoercibleEff e e', Member e' r)
              => (forall x. z x -> Sem r x)
              -> e z a -> Sem r a
coerceSendVia = coerceSendViaUsing @e' membership
{-# INLINE coerceSendVia #-}

coerceSendViaUsing :: forall e' e z r a
                    . CoercibleEff e e'
                   => ElemOf e' r
                   -> (forall x. z x -> Sem r x)
                   -> e z a -> Sem r a
coerceSendViaUsing pr n = sendViaUsing pr n .# coerce
{-# INLINE coerceSendViaUsing #-}

subsumeCoerce :: forall e' e r a
               . (CoercibleEff e e', Member e' r)
              => Sem (e ': r) a
              -> Sem r a
subsumeCoerce = subsumeCoerceUsing @e' membership
{-# INLINE subsumeCoerce #-}

subsumeCoerceUsing :: forall e' e r a
                    . CoercibleEff e e'
                   => ElemOf e' r
                   -> Sem (e ': r) a -> Sem r a
subsumeCoerceUsing pr = subsumeUsing @e' pr . coerceEff @e' @e
{-# INLINE subsumeCoerceUsing #-}
