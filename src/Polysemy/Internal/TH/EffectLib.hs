{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

module Polysemy.Internal.TH.EffectLib
  ( ConLiftInfo (..)
  , getEffectMetadata
  , checkExtensions
  , foldArrows
  ) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Char (toLower)
import           Data.Either
import           Data.Generics hiding (Fixity)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Tuple
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.PprLib
import           Polysemy.Internal (Sem)
import           Prelude hiding ((<>))


getEffectMetadata :: Name -> Q (DatatypeInfo, [ConLiftInfo])
getEffectMetadata type_name = do
  dt_info  <- reifyDatatype type_name
  cl_infos <- traverse (mkCLInfo dt_info) $ datatypeCons dt_info
  pure (dt_info, cl_infos)


------------------------------------------------------------------------------
-- | Info about constructor being lifted; use 'mkCLInfo' to create one.
data ConLiftInfo = CLInfo
  { -- | Name of effect's type constructor
    cliEffName   :: Name
    -- | Effect-specific type arguments
  , cliEffArgs   :: [Type]
    -- | Result type specific to action
  , cliResType   :: Type
    -- | Name of action constructor
  , cliConName   :: Name
    -- | Name of final function
  , cliFunName   :: Name
    -- | Fixity of function used as an operator
  , cliFunFixity :: Maybe Fixity
    -- | Final function arguments
  , cliArgs   :: [(Name, Type)]
    -- | Constraints of final function
  , cliFunCxt    :: Cxt
    -- | Name of type variable parameterizing 'Sem'
  , cliUnionName :: Name
  } deriving Show


------------------------------------------------------------------------------
-- | Creates info about smart constructor being created from info about action
-- and it's parent type.
mkCLInfo :: DatatypeInfo -> ConstructorInfo -> Q ConLiftInfo
mkCLInfo dti ci = do
  let cliEffName            = datatypeName dti

  (raw_cli_eff_args, [m_arg, raw_cli_res_arg]) <-
    case splitAtEnd 2 $ datatypeInstTypes dti of
      r@(_, [_, _]) -> pure r
      _             -> missingEffArgs cliEffName

  m_name <-
    case tVarName m_arg of
      Just r        -> pure r
      Nothing       -> mArgNotVar cliEffName m_arg

  cliUnionName <- newName "r"
  cliFunFixity <- reifyFixity $ constructorName ci

  let normalizeType         = replaceMArg m_name cliUnionName
                            . simplifyKinds
                            . applySubstitution eq_pairs
      -- We extract equality constraints with variables to unify them
      -- manually - this makes type errors more readable. Plus we replace
      -- kind of result with 'Type' if it is a type variable.
      (eq_pairs, cliFunCxt) = first (M.fromList . maybeResKindToType)
                            $ partitionEithers
                            $ eqPairOrCxt <$> constructorContext ci
      maybeResKindToType    = maybe id (\k ps -> (k, StarT) : ps)
                            $ tVarName $ tvKind $ last
                            $ datatypeVars dti

      cliEffArgs            = normalizeType <$> raw_cli_eff_args
      cliResType            = normalizeType     raw_cli_res_arg
      cliConName            = constructorName ci
      cliFunName            = liftFunNameFromCon cliConName
      arg_types             = normalizeType <$> constructorFields ci

  arg_names <- replicateM (length arg_types) $ newName "x"

  pure CLInfo{cliArgs = zip arg_names arg_types, ..}


------------------------------------------------------------------------------
-- Error messages and checks

mArgNotVar :: Name -> Type -> Q a
mArgNotVar name mArg = fail $ show
  $  text "Monad argument ‘" <> ppr mArg <> text "’ in effect ‘"
  <> ppr name <> text "’ is not a type variable"


missingEffArgs :: Name -> Q a
missingEffArgs name = fail $ show
  $   text "Effect ‘" <> ppr name
      <> text "’ has not enough type arguments"
  $+$ nest 4
      (   text "At least monad and result argument are required, e.g.:"
      $+$ nest 4
          (   text ""
          $+$ ppr (DataD [] base args Nothing [] []) <+> text "..."
          $+$ text ""
          )
      )
  where
    base = capturableBase name
    args = PlainTV . mkName <$> ["m", "a"]


checkExtensions :: [Extension] -> Q ()
checkExtensions exts = do
  states <- zip exts <$> traverse isExtEnabled exts
  maybe (pure ())
        (\(ext, _) -> fail $ show
          $ char '‘' <> text (show ext) <> char '’'
            <+> text "extension needs to be enabled\
                     \ for Polysemy's Template Haskell to work")
        (find (not . snd) states)

------------------------------------------------------------------------------
-- | Constructs capturable name from base of input name.
capturableBase :: Name -> Name
capturableBase = mkName . nameBase

------------------------------------------------------------------------------
-- | Replaces use of @m@ in type with @Sem r@.
replaceMArg :: TypeSubstitution t => Name -> Name -> t -> t
replaceMArg m r = applySubstitution $ M.singleton m $ ConT ''Sem `AppT` VarT r

------------------------------------------------------------------------------
-- Removes 'Type' and variable kind signatures from type.
simplifyKinds :: Type -> Type
simplifyKinds = everywhere $ mkT $ \case
  SigT t StarT    -> t
  SigT t VarT{}   -> t
  ForallT bs cs t -> ForallT (goBndr <$> bs) (simplifyKinds <$> cs) t
    where
      goBndr (KindedTV n StarT ) = PlainTV n
      goBndr (KindedTV n VarT{}) = PlainTV n
      goBndr b = b
  t -> t

------------------------------------------------------------------------------
-- | Converts equality constraint with type variable to name and type pair if
-- possible or leaves constraint as is.
eqPairOrCxt :: Pred -> Either (Name, Type) Pred
eqPairOrCxt p = case asEqualPred p of
  Just (VarT n, b) -> Left (n, b)
  Just (a, VarT n) -> Left (n, a)
  _                -> Right p

------------------------------------------------------------------------------
-- | Creates name of lifting function from action name.
liftFunNameFromCon :: Name -> Name
liftFunNameFromCon n = mkName $ case nameBase n of
                         ':':cs -> cs
                         c  :cs -> toLower c : cs
                         ""     -> error
                           "liftFunNameFromCon: empty constructor name"

------------------------------------------------------------------------------
-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 $ AppT . AppT ArrowT

------------------------------------------------------------------------------
-- | Extracts name from type variable (possibly nested in signature and/or
-- some context), returns 'Nothing' otherwise.
tVarName :: Type -> Maybe Name
tVarName = \case
  ForallT _ _ t -> tVarName t
  SigT t _      -> tVarName t
  VarT n        -> Just n
  ParensT t     -> tVarName t
  _             -> Nothing

------------------------------------------------------------------------------
-- | 'splitAt' counting from the end.
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n = swap . join bimap reverse . splitAt n . reverse

