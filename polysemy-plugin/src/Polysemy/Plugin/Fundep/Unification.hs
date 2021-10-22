{-# LANGUAGE CPP #-}

module Polysemy.Plugin.Fundep.Unification where

import           Data.Bool
import           Data.Function (on)
import           Data.Set (Set)
import qualified Data.Set as S
#if __GLASGOW_HASKELL__ >= 900
import           GHC.Tc.Types.Constraint
#elif __GLASGOW_HASKELL__ >= 810
import           Constraint
#else
import           TcRnTypes
#endif

#if __GLASGOW_HASKELL__ >= 900
import           GHC.Core.Type
import           GHC.Core.Unify
#else
import           Type
import           Unify
#endif


------------------------------------------------------------------------------
-- | The context in which we're attempting to solve a constraint.
data SolveContext
  = -- | In the context of a function definition. The @Set TyVar@ is all of the
    -- skolems that exist in the [G] constraints for this function.
    FunctionDef (Set TyVar)
    -- | In the context of running an interpreter. The 'Bool' corresponds to
    -- whether we are only trying to solve a single 'Member' constraint right
    -- now. If so, we *must* produce a unification wanted.
  | InterpreterUse Bool
  deriving (Eq, Ord)


------------------------------------------------------------------------------
-- | Depending on the context in which we're solving a constraint, we may or
-- may not want to force a unification of effects. For example, when defining
-- user code whose type is @Member (State Int) r => ...@, if we see @get :: Sem
-- r s@, we should unify @s ~ Int@.
mustUnify :: SolveContext -> Bool
mustUnify (FunctionDef _) = True
mustUnify (InterpreterUse b) = b


------------------------------------------------------------------------------
-- | Determine whether or not two effects are unifiable.
--
-- All free variables in [W] constraints are considered skolems, and thus are
-- not allowed to unify with anything but themselves. This properly handles all
-- cases in which we are unifying ambiguous [W] constraints (which are true
-- type variables) against [G] constraints.
unify
    :: SolveContext
    -> Type  -- ^ wanted
    -> Type  -- ^ given
    -> Maybe TCvSubst
unify solve_ctx = tryUnifyUnivarsButNotSkolems skolems
  where
    skolems :: Set TyVar
    skolems =
      case solve_ctx of
        InterpreterUse _ -> mempty
        FunctionDef s    -> s


tryUnifyUnivarsButNotSkolems :: Set TyVar -> Type -> Type -> Maybe TCvSubst
tryUnifyUnivarsButNotSkolems skolems goal inst =
  case tcUnifyTysFG
         (bool BindMe Skolem . flip S.member skolems)
         [inst]
         [goal] of
    Unifiable subst -> pure subst
    _               -> Nothing


------------------------------------------------------------------------------
-- | A wrapper for two types that we want to say have been unified.
data Unification = Unification
  { _unifyLHS :: OrdType
  , _unifyRHS :: OrdType
  }
  deriving (Eq, Ord)


------------------------------------------------------------------------------
-- | 'Type's don't have 'Eq' or 'Ord' instances by default, even though there
-- are functions in GHC that implement these operations. This newtype gives us
-- those instances.
newtype OrdType = OrdType
  { getOrdType :: Type
  }

instance Eq OrdType where
  (==) = eqType `on` getOrdType

instance Ord OrdType where
  compare = nonDetCmpType `on` getOrdType


------------------------------------------------------------------------------
-- | Filter out the unifications we've already emitted, and then give back the
-- things we should put into the @S.Set Unification@, and the new constraints
-- we should emit.
unzipNewWanteds
    :: S.Set Unification
    -> [(Unification, Ct)]
    -> ([Unification], [Ct])
unzipNewWanteds old = unzip . filter (not . flip S.member old . fst)

