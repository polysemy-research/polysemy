{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.CustomErrors
  ( AmbiguousSend
  , WhenStuck
  , FirstOrder
  , UnhandledEffect
  , DefiningModule
  , DefiningModuleForEffect
  ) where

import Data.Kind
import Fcf
import GHC.TypeLits (Symbol)
import Polysemy.Internal.Kind
import Polysemy.Internal.CustomErrors.Redefined
import Type.Errors hiding (IfStuck, WhenStuck, UnlessStuck)
import Type.Errors.Pretty (type (<>), type (%))


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


-- TODO(sandy): Put in type-errors
type ShowTypeBracketed t = "(" <> t <> ")"


------------------------------------------------------------------------------
-- | The constructor of the effect row --- it's either completely polymorphic,
-- a nil, or a cons.
data EffectRowCtor = TyVarR | NilR | ConsR


------------------------------------------------------------------------------
-- | Given that @r@ isn't stuck, determine which constructor it has.
type family UnstuckRState (r :: EffectRow) :: EffectRowCtor where
  UnstuckRState '[]      = 'NilR
  UnstuckRState (_ ': _) = 'ConsR


------------------------------------------------------------------------------
-- | Put brackets around @r@ if it's a cons.
type family ShowRQuoted (rstate :: EffectRowCtor) (r :: EffectRow) :: ErrorMessage where
  ShowRQuoted 'TyVarR r = 'ShowType r
  ShowRQuoted 'NilR   r = 'ShowType r
  ShowRQuoted 'ConsR  r = ShowTypeBracketed r


type AmbigousEffectMessage (rstate :: EffectRowCtor)
                           (r :: EffectRow)
                           (e :: k)
                           (t :: Effect)
                           (vs :: [Type])
  = "Ambiguous use of effect '" <> e <> "'"
  % "Possible fix:"
  % "  add (Member (" <> t <> ") " <> ShowRQuoted rstate r <> ") to the context of "
  % "    the type signature"
  % "If you already have the constraint you want, instead"
  % "  add a type application to specify"
  % "    " <> PrettyPrintList vs <> " directly, or activate polysemy-plugin which"
  % "      can usually infer the type correctly."

type AmbiguousSend e r =
      (IfStuck r
        (AmbiguousSendError 'TyVarR r e)
        (Pure (AmbiguousSendError (UnstuckRState r) r e)))


type family AmbiguousSendError rstate r e where
  AmbiguousSendError rstate r (e a b c d f) =
    TypeError (AmbigousEffectMessage rstate r e (e a b c d f) '[a, b c d f])

  AmbiguousSendError rstate r (e a b c d) =
    TypeError (AmbigousEffectMessage rstate r e (e a b c d) '[a, b c d])

  AmbiguousSendError rstate r (e a b c) =
    TypeError (AmbigousEffectMessage rstate r e (e a b c) '[a, b c])

  AmbiguousSendError rstate r (e a b) =
    TypeError (AmbigousEffectMessage rstate r e (e a b) '[a, b])

  AmbiguousSendError rstate r (e a) =
    TypeError (AmbigousEffectMessage rstate r e (e a) '[a])

  AmbiguousSendError rstate r e =
    TypeError
        ( "Could not deduce: (Member " <>  e <> " " <> ShowRQuoted rstate r <> ") "
        % "Fix:"
        % "  add (Member " <>  e <> " " <> r <> ") to the context of"
        % "    the type signature"
        )


data FirstOrderErrorFcf :: k -> Symbol -> Exp Constraint
type instance Eval (FirstOrderErrorFcf e fn) = $(te[t|
    UnlessPhantom
        (e PHANTOM)
        ( "'" <> e <> "' is higher-order, but '" <> fn <> "' can help only"
        % "with first-order effects."
        % "Fix:"
        % "  use '" <> fn <> "H' instead."
        ) |])

------------------------------------------------------------------------------
-- | This constraint gives helpful error messages if you attempt to use a
-- first-order combinator with a higher-order type.
type FirstOrder (e :: Effect) fn = UnlessStuck e (FirstOrderErrorFcf e fn)


------------------------------------------------------------------------------
-- | Unhandled effects
type UnhandledEffectMsg e
  = "Unhandled effect '" <> e <> "'"
  % "Probable fix:"
  % "  add an interpretation for '" <> e <> "'"

type CheckDocumentation e
  = "  If you are looking for inspiration, try consulting"
  % "    the documentation for module '" <> DefiningModuleForEffect e <> "'"

type family UnhandledEffect e where
  UnhandledEffect e =
    IfStuck (DefiningModule e)
            (TypeError (UnhandledEffectMsg e))
            (DoError (UnhandledEffectMsg e ':$$: CheckDocumentation e))


data DoError :: ErrorMessage -> Exp k
type instance Eval (DoError a) = TypeError a
