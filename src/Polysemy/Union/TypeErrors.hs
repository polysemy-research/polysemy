{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Polysemy.Union.TypeErrors
  ( AmbiguousSend
  , Break
  , FirstOrder
  ) where

import Data.Coerce
import Data.Kind
import GHC.TypeLits


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
    ':$$: 'Text "If you are seeing this error at the interpretation site,"
    ':$$: 'Text "  it means you forgot to handle the '"
    ':<>: 'ShowType t
    ':<>: 'Text "' effect"
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
          ':<>: 'Text "' is only"
          ':$$: 'Text "helpful for first-order effects."
          ':$$: 'Text "Fix:"
          ':$$: 'Text "  use '"
          ':<>: 'Text fn
          ':<>: 'Text "H' instead."
              )

type FirstOrder e fn = ∀ m. Coercible (e m) (e (FirstOrderError e fn))

