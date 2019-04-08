{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Polysemy.Union.TypeErrors
  ( AmbiguousSend
  , Break
  , FirstOrder
  , UnhandledEffect
  , DefiningModule
  , DefiningModuleForEffect
  ) where

import Data.Coerce
import Data.Kind
import GHC.TypeLits


type family DefiningModule (t :: k) :: Symbol

type family DefiningModuleForEffect (e :: k) :: Symbol where
  DefiningModuleForEffect (e a) = DefiningModuleForEffect e
  DefiningModuleForEffect e     = DefiningModule e



data T1 m a

type family Break (c :: Constraint)
                  (rep :: (* -> *) -> * -> *) :: Constraint where
  Break _ T1 = ((), ())
  Break _ c  = ()



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
    ':<>: 'Text " directly"
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

type FirstOrder e fn = ∀ m. Coercible (e m) (e (FirstOrderError e fn))


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

