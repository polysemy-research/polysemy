{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.CustomErrors
  ( AmbiguousSend
  , IfStuck
  , WhenStuck
  , FirstOrder
  , UnhandledEffect
  , DefiningModule
  , DefiningModuleForEffect
  ) where

import Data.Coerce
import Data.Kind
import Fcf
import GHC.TypeLits
import Polysemy.Internal.Kind


type family DefiningModule (t :: k) :: Symbol

type family DefiningModuleForEffect (e :: k) :: Symbol where
  DefiningModuleForEffect (e a) = DefiningModuleForEffect e
  DefiningModuleForEffect e     = DefiningModule e


data T1 m a


type family IfStuck (tyvar :: k) (b :: k1) (c :: Exp k1) :: k1 where
  -- TODO(sandy): This behavior is wrong when k is not one of these things
  IfStuck T1                  b c = b
  IfStuck "NOT_A_REAL_SYMBOL" b c = b
  IfStuck 9999999999999999999 b c = b
  IfStuck a                   b c = Eval c

type WhenStuck a b = IfStuck a b (Pure (() :: Constraint))


type AmbigousEffectMessage r e t vs =
        ( 'Text "Ambiguous use of effect '"
    ':<>: 'ShowType e
    ':<>: 'Text "'"
    ':$$: 'Text "Possible fix:"
    ':$$: 'Text "  add (Member ("
    ':<>: 'ShowType t
    ':<>: 'Text ") "
    ':<>: 'ShowType r
    ':<>: 'Text ") to the context of "
    ':$$: 'Text "    the type signature"
    ':$$: 'Text "If you already have the constraint you want, instead"
    ':$$: 'Text "  add a type application to specify"
    ':$$: 'Text "    "
    ':<>: PrettyPrint vs
    ':<>: 'Text " directly, or activate polysemy-plugin which"
    ':$$: 'Text "      can usually infer the type correctly."
        )

type family PrettyPrint (vs :: [k]) where
  PrettyPrint '[a] =
    'Text "'" ':<>: 'ShowType a ':<>: 'Text "'"
  PrettyPrint '[a, b] =
    'Text "'" ':<>: 'ShowType a ':<>: 'Text "', and "
    ':<>:
    'Text "'" ':<>: 'ShowType b ':<>: 'Text "'"
  PrettyPrint (a ': vs) =
    'Text "'" ':<>: 'ShowType a ':<>: 'Text "', "
    ':<>: PrettyPrint vs


type family AmbiguousSend r e where
  AmbiguousSend r (e a b c d f) =
    TypeError (AmbigousEffectMessage r e (e a b c d f) '[a, b c d f])

  AmbiguousSend r (e a b c d) =
    TypeError (AmbigousEffectMessage r e (e a b c d) '[a, b c d])

  AmbiguousSend r (e a b c) =
    TypeError (AmbigousEffectMessage r e (e a b c) '[a, b c])

  AmbiguousSend r (e a b) =
    TypeError (AmbigousEffectMessage r e (e a b) '[a, b])

  AmbiguousSend r (e a) =
    TypeError (AmbigousEffectMessage r e (e a) '[a])

  AmbiguousSend r e =
    TypeError
        ( 'Text "Could not deduce: (Member "
    ':<>: 'ShowType e
    ':<>: 'Text " "
    ':<>: 'ShowType r
    ':<>: 'Text ") "
    ':$$: 'Text "Fix:"
    ':$$: 'Text "  add (Member "
    ':<>: 'ShowType e
    ':<>: 'Text " "
    ':<>: 'ShowType r
    ':<>: 'Text ") to the context of"
    ':$$: 'Text "    the type signature"
        )





type family FirstOrderError e (fn :: Symbol) :: k where
  FirstOrderError e fn =
    TypeError ( 'Text "'"
          ':<>: 'ShowType e
          ':<>: 'Text "' is higher-order, but '"
          ':<>: 'Text fn
          ':<>: 'Text "' can help only"
          ':$$: 'Text "with first-order effects."
          ':$$: 'Text "Fix:"
          ':$$: 'Text "  use '"
          ':<>: 'Text fn
          ':<>: 'Text "H' instead."
              )

------------------------------------------------------------------------------
-- | This constraint gives helpful error messages if you attempt to use a
-- first-order combinator with a higher-order type.
--
-- Note that the parameter 'm' is only required to work around supporting
-- versions of GHC without QuantifiedConstraints
type FirstOrder m e fn = IfStuck e (() :: Constraint) (FirstOrderFcf m e fn)

data FirstOrderFcf
    :: (Type -> Type)
    -> Effect
    -> Symbol
    -> Exp Constraint
type instance Eval (FirstOrderFcf m e fn) = Coercible (e m) (e (FirstOrderError e fn))


------------------------------------------------------------------------------
-- | Unhandled effects
type UnhandledEffectMsg e
      = 'Text "Unhandled effect '"
  ':<>: 'ShowType e
  ':<>: 'Text "'"
  ':$$: 'Text "Probable fix:"
  ':$$: 'Text "  add an interpretation for '"
  ':<>: 'ShowType e
  ':<>: 'Text "'"

type CheckDocumentation e
      = 'Text "  If you are looking for inspiration, try consulting"
  ':$$: 'Text "    the documentation for module '"
  ':<>: 'Text (DefiningModuleForEffect e)
  ':<>: 'Text "'"

type family UnhandledEffect e where
  UnhandledEffect e =
    IfStuck (DefiningModule e)
            (TypeError (UnhandledEffectMsg e))
            (DoError (UnhandledEffectMsg e ':$$: CheckDocumentation e))


data DoError :: ErrorMessage -> Exp k
type instance Eval (DoError a) = TypeError a

