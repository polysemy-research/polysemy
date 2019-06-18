{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

#if __GLASGOW_HASKELL__ <= 806
{-# LANGUAGE TypeInType #-}
#endif

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.CustomErrors
  ( AmbiguousSend
  , Break
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

type family Break (c :: Constraint)
                  (rep :: Effect) :: Constraint where
  Break _ T1 = ((), ())
  Break _ c  = ()


type family IfStuck (tyvar :: k) (b :: k1) (c :: Exp k1) :: k1 where
  IfStuck T1 b c = b
  IfStuck a  b c = Eval c


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

type family BreakSym (z :: k -> k) e (c :: Constraint)
                       (rep :: Symbol) :: k where
  BreakSym _ e _ "" =  TypeError (UnhandledEffectMsg e ':$$: CheckDocumentation e)
  BreakSym z e _ c  = z (TypeError (UnhandledEffectMsg e ':$$: CheckDocumentation e))

type family UnhandledEffect z e where
  UnhandledEffect z e =
    BreakSym z e (TypeError (UnhandledEffectMsg e)) (DefiningModuleForEffect e)

