{-# LANGUAGE CPP             #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns    #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.TH.Common
  ( ConLiftInfo (..)
  , getEffectMetadata
  , makeMemberConstraint
  , makeMemberConstraint'
  , makeSemType
  , makeInterpreterType
  , makeEffectType
  , makeUnambiguousSend
  , checkExtensions
  , foldArrowTs
  , splitArrowTs
  , pattern (:->)
  ) where

import           Control.Arrow ((>>>))
import           Control.Monad
import           Data.Bifunctor
import           Data.Char (toLower)
import           Data.Generics hiding (Fixity)
import           Data.List
import qualified Data.Map.Strict as M
import           Data.Tuple
import           Language.Haskell.TH
import           Language.Haskell.TH.Datatype
import           Language.Haskell.TH.PprLib
import           Polysemy.Internal (Sem, send)
import           Polysemy.Internal.Union (MemberWithError)

#if __GLASGOW_HASKELL__ >= 804
import           Prelude hiding ((<>))
#endif


------------------------------------------------------------------------------
-- Effects TH ----------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Info about constructor being lifted; use 'makeCLInfo' to create one.
data ConLiftInfo = CLInfo
  { -- | Name of effect's type constructor
    cliEffName   :: Name
  , -- | Effect-specific type arguments
    cliEffArgs   :: [Type]
  , -- | Result type specific to action
    cliEffRes    :: Type
  , -- | Name of action constructor
    cliConName   :: Name
  , -- | Name of final function
    cliFunName   :: Name
  , -- | Fixity of function used as an operator
    cliFunFixity :: Maybe Fixity
  , -- | Final function arguments
    cliFunArgs   :: [(Name, Type)]
  , -- | Constraints of final function
    cliFunCxt    :: Cxt
  , -- | Name of type variable parameterizing 'Sem'
    cliUnionName :: Name
  } deriving Show


------------------------------------------------------------------------------
-- | Given an name of datatype or some of it's constructors/fields, return
-- datatype's name together with info about it's constructors.
getEffectMetadata :: Name -> Q (Name, [ConLiftInfo])
getEffectMetadata type_name = do
  dt_info  <- reifyDatatype type_name
  cl_infos <- traverse makeCLInfo $ constructorName <$> datatypeCons dt_info
  pure (datatypeName dt_info, cl_infos)


------------------------------------------------------------------------------
-- | Creates name of lifting function from action name.
liftFunNameFromCon :: Name -> Name
liftFunNameFromCon n = mkName $
  case nameBase n of
    ':' : cs -> cs
    c   : cs -> toLower c : cs
    ""       -> error "liftFunNameFromCon: empty constructor name"


------------------------------------------------------------------------------
-- | Creates info about smart constructor being created from name of the
-- original one.
makeCLInfo :: Name -> Q ConLiftInfo
makeCLInfo cliConName = do
  (con_type, cliEffName) <- reify cliConName >>= \case
    DataConI _ t p -> pure (t, p)
    _              -> notDataCon cliConName

  let (con_args, [con_return_type]) = splitAtEnd 1
                                    $ splitArrowTs con_type

  (ty_con_args, [monad_arg, res_arg]) <-
    case splitAtEnd 2 $ tail $ splitAppTs $ con_return_type of
      r@(_, [_, _]) -> pure r
      _             -> missingEffArgs cliEffName

  monad_name   <- maybe (argNotVar cliEffName monad_arg)
                        pure
                        (tVarName monad_arg)

  cliUnionName <- newName "r"

  let normalize_types :: (TypeSubstitution t, Data t) => t -> t
      normalize_types = replaceMArg monad_name cliUnionName
                      . simplifyKinds

      cliEffArgs      = normalize_types ty_con_args
      cliEffRes       = normalize_types res_arg
      cliFunName      = liftFunNameFromCon cliConName

  cliFunFixity  <- reifyFixity cliConName

  fun_arg_names <- replicateM (length con_args) $ newName "x"

  let cliFunArgs    = zip fun_arg_names $ normalize_types con_args
      -- GADTs seem to forbid constraints further in signature, so top level
      -- ones should be fine.
      cliFunCxt     = topLevelConstraints con_type

  pure CLInfo{..}


------------------------------------------------------------------------------
-- | Given a 'ConLiftInfo', get the corresponding effect type.
makeEffectType :: ConLiftInfo -> Type
makeEffectType cli = foldl' AppT (ConT $ cliEffName cli) $ cliEffArgs cli


------------------------------------------------------------------------------
-- | @'makeInterpreterType' con r a@ will produce a @'Polysemy.Sem' (Effect ':
-- r) a -> 'Polysemy.Sem' r a@ type, where @Effect@ is the effect
-- corresponding to the 'ConLiftInfo' for @con@.
makeInterpreterType :: ConLiftInfo -> Name -> Type -> Type
makeInterpreterType cli r result = sem_with_eff :-> makeSemType r result where
  sem_with_eff = ConT ''Sem `AppT` r_with_eff `AppT` result
  r_with_eff   = PromotedConsT `AppT` makeEffectType cli `AppT` VarT r


------------------------------------------------------------------------------
-- | Turn a 'ConLiftInfo' for @Foo@ into a @Member Foo r@ constraint.
makeMemberConstraint :: Name -> ConLiftInfo -> Pred
makeMemberConstraint r cli = makeMemberConstraint' r $ makeEffectType cli


------------------------------------------------------------------------------
-- | @'makeMemberConstraint'' r type@ will produce a @Member type r@
-- constraint.
makeMemberConstraint' :: Name -> Type -> Pred
makeMemberConstraint' r eff = classPred ''MemberWithError [eff, VarT r]


------------------------------------------------------------------------------
-- | @'makeSemType' r a@ will produce a @'Polysemy.Sem' r a@ type.
makeSemType :: Name -> Type -> Type
makeSemType r result = ConT ''Sem `AppT` VarT r `AppT` result


------------------------------------------------------------------------------
-- | Given a 'ConLiftInfo', this will produce an action for it. It's arguments
-- will come from any variables in scope that correspond to the 'cliArgs' of
-- the 'ConLiftInfo'.
makeUnambiguousSend :: Bool -> ConLiftInfo -> Exp
makeUnambiguousSend should_make_sigs cli =
  let fun_args_names = fmap fst $ cliFunArgs cli
      action = foldl1' AppE
             $ ConE (cliConName cli) : (VarE <$> fun_args_names)
      eff    = foldl' AppT (ConT $ cliEffName cli) $ args
               -- see NOTE(makeSem_)
      args   = (if should_make_sigs then id else map capturableTVars)
             $ cliEffArgs cli ++ [sem, cliEffRes cli]
      sem    = ConT ''Sem `AppT` VarT (cliUnionName cli)
   in AppE (VarE 'send) $ SigE action eff


-- Error messages and checks -------------------------------------------------

argNotVar :: Name -> Type -> Q a
argNotVar eff_name arg = fail $ show
  $ text "Argument ‘" <> ppr arg <> text "’ in effect ‘" <> ppr eff_name
    <> text "’ is not a type variable"

-- | Fail the 'Q' monad whenever the given 'Extension's aren't enabled in the
-- current module.
checkExtensions :: [Extension] -> Q ()
checkExtensions exts = do
  states <- zip exts <$> traverse isExtEnabled exts
  maybe (pure ())
        (\(ext, _) -> fail $ show
          $ char '‘' <> text (show ext) <> char '’'
            <+> text "extension needs to be enabled for Polysemy's Template Haskell to work")
        (find (not . snd) states)

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

notDataCon :: Name -> Q a
notDataCon name = fail $ show
  $ char '‘' <> ppr name <> text "’ is not a data constructor"


------------------------------------------------------------------------------
-- TH utilities --------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Pattern constructing function type and matching on one that may contain
-- type annotations on arrow itself.
infixr 1 :->
pattern (:->) :: Type -> Type -> Type
pattern a :-> b <- (removeTyAnns -> ArrowT) `AppT` a `AppT` b where
  a :-> b = ArrowT `AppT` a `AppT` b


------------------------------------------------------------------------------
-- | Constructs capturable name from base of input name.
capturableBase :: Name -> Name
capturableBase = mkName . nameBase


------------------------------------------------------------------------------
-- | Converts names of all type variables in type to capturable ones based on
-- original name base. Use with caution, may create name conflicts!
capturableTVars :: Type -> Type
capturableTVars = everywhere $ mkT $ \case
  VarT n          -> VarT $ capturableBase n
  ForallT bs cs t -> ForallT (goBndr <$> bs) (capturableTVars <$> cs) t
    where
      goBndr (PlainTV n   ) = PlainTV $ capturableBase n
      goBndr (KindedTV n k) = KindedTV (capturableBase n) $ capturableTVars k
  t -> t


------------------------------------------------------------------------------
-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrowTs :: Type -> [Type] -> Type
foldArrowTs = foldr (:->)


------------------------------------------------------------------------------
-- | Replaces use of @m@ in type with @Sem r@.
replaceMArg :: TypeSubstitution t => Name -> Name -> t -> t
replaceMArg m r = applySubstitution $ M.singleton m $ ConT ''Sem `AppT` VarT r


------------------------------------------------------------------------------
-- Removes 'Type' and variable kind signatures from type.
simplifyKinds :: Data t => t -> t
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
splitAppTs :: Type -> [Type]
splitAppTs = removeTyAnns >>> \case
  t `AppT` arg -> splitAppTs t ++ [arg]
  t            -> [t]


------------------------------------------------------------------------------
splitArrowTs :: Type -> [Type]
splitArrowTs = removeTyAnns >>> \case
  t :-> ts -> t : splitArrowTs ts
  t        -> [t]


------------------------------------------------------------------------------
-- | Extracts name from type variable (possibly nested in signature and/or
-- some context), returns 'Nothing' otherwise.
tVarName :: Type -> Maybe Name
tVarName = removeTyAnns >>> \case
  VarT n -> Just n
  _      -> Nothing


------------------------------------------------------------------------------
topLevelConstraints :: Type -> Cxt
topLevelConstraints = \case
  ForallT _ cs _ -> cs
  _              -> []


------------------------------------------------------------------------------
removeTyAnns :: Type -> Type
removeTyAnns = \case
  ForallT _ _ t -> removeTyAnns t
  SigT t _      -> removeTyAnns t
  ParensT t     -> removeTyAnns t
  t -> t


------------------------------------------------------------------------------
-- Miscellaneous -------------------------------------------------------------
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | 'splitAt' counting from the end.
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n = swap . join bimap reverse . splitAt n . reverse
