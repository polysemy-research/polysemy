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
import Type.Errors


------------------------------------------------------------------------------
-- | The module this effect was originally defined in. This type family is used
-- only for providing better error messages.
--
-- Calls to 'Polysemy.Internal.TH.Effect.makeSem' will automatically give
-- instances of 'DefiningModule'.
type family DefiningModule (t :: k) :: Symbol

type family DefiningModuleForEffect (e :: k) :: Symbol where
  DefiningModuleForEffect (e a) = DefiningModuleForEffect e
  DefiningModuleForEffect e     = DefiningModule e


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
    ':<>: PrettyPrintList vs
    ':<>: 'Text " directly, or activate polysemy-plugin which"
    ':$$: 'Text "      can usually infer the type correctly."
        )


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





type family FirstOrderError e (fn :: Symbol) :: ErrorMessage where
  FirstOrderError e fn =
    ( 'Text "'"
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
type FirstOrder (e :: Effect) fn = UnlessStuck e (UnlessPhantomFcf (Type -> Type) (e (PHANTOM :: Type -> Type)) (FirstOrderError e fn))


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

