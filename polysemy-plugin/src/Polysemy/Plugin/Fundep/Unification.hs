module Polysemy.Plugin.Fundep.Unification where

import           Data.Bool
import           Data.Function (on)
import qualified Data.Set as S
import           TcRnTypes
import           Type


------------------------------------------------------------------------------
-- | The context in which we're attempting to solve a constraint.
data SolveContext
  = -- | In the context of a function definition.
    FunctionDef
    -- | In the context of running an interpreter. The 'Bool' corresponds to
    -- whether we are only trying to solve a single 'Member' constraint right
    -- now. If so, we *must* produce a unification wanted.
  | InterpreterUse Bool
  deriving (Eq, Ord, Show)


mustUnify :: SolveContext -> Bool
mustUnify FunctionDef = True
mustUnify (InterpreterUse b) = b


canUnifyRecursive :: SolveContext -> Type -> Type -> Bool
canUnifyRecursive solve_ctx = go True
  where
    -- It's only OK to solve a polymorphic "given" if we're in the context of
    -- an interpreter, because it's not really a given!
    poly_given_ok :: Bool
    poly_given_ok =
      case solve_ctx of
        InterpreterUse _ -> True
        FunctionDef      -> False

    -- On the first go around, we don't want to unify effects with tyvars, but
    -- we _do_ want to unify their arguments, thus 'is_first'.
    go :: Bool -> Type -> Type -> Bool
    go is_first wanted given =
      let (w, ws) = splitAppTys wanted
          (g, gs) = splitAppTys given
       in (&& bool (canUnify poly_given_ok) eqType is_first w g)
        . flip all (zip ws gs)
        $ \(wt, gt) -> canUnify poly_given_ok wt gt || go False wt gt


canUnify :: Bool -> Type -> Type -> Bool
canUnify poly_given_ok wt gt =
  or [ isTyVarTy wt
     , isTyVarTy gt && poly_given_ok
     , eqType wt gt
     ]


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


unzipNewWanteds :: S.Set Unification -> [(Unification, Ct)] -> ([Unification], [Ct])
unzipNewWanteds old = unzip . filter (not . flip S.member old . fst)

