{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE NamedFieldPuns      #-}

-- | This module provides Template Haskell functions for automatically generating
-- effect operation functions (that is, functions that use 'send') from a given
-- effect algebra. For example, using the @FileSystem@ effect from the example in
-- the module documentation for "Polysemy", we can write the following:
--
-- @
-- data FileSystem m a where
--   ReadFile :: 'FilePath' -> FileSystem 'String'
--   WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
-- 'makeSem' ''FileSystem
-- @
--
-- This will automatically generate the following functions:
--
-- @
-- readFile :: 'Member' FileSystem r => 'FilePath' -> 'Sem' r 'String'
-- readFile a = 'send' (ReadFile a)
--
-- writeFile :: 'Member' FileSystem r => 'FilePath' -> 'String' -> 'Sem' r ()
-- writeFile a b = 'send' (WriteFile a b)
-- @
module Polysemy.Internal.TH.Effect
  ( makeSem
  , makeSem_
  ) where

import Prelude hiding ((<>))

import Control.Monad
import Data.Bifunctor
import Data.Char                      (toLower)
import Data.Either
import Data.Generics
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Datatype
import Polysemy.Internal              (send, Member, Sem)
import Polysemy.Internal.CustomErrors (DefiningModule)

import qualified Data.Map.Strict as M

-- TODO: update documentation based on other decisions
-- TODO: write tests for what should (not) compile

------------------------------------------------------------------------------
-- | If @T@ is a GADT representing an effect algebra, as described in the
-- module documentation for "Polysemy", @$('makeSem' ''T)@ automatically
-- generates a smart constructor for every data constructor of @T@.
--
-- @since 0.1.2.0
makeSem :: Name -> Q [Dec]
makeSem = genFreer True

-- | Like 'makeSem', but does not provide type signatures. This can be used
-- to attach Haddock comments to individual arguments for each generated
-- function.
--
-- @
-- data Lang m a where
--   Output :: String -> Lang ()
--
-- makeSem_ ''Lang
--
-- -- | Output a string.
-- output :: Member Lang r
--        => String         -- ^ String to output.
--        -> Sem r ()  -- ^ No result.
-- @
--
-- Note that 'makeSem_' must be used /before/ the explicit type signatures.
--
-- @since 0.1.2.0
makeSem_ :: Name -> Q [Dec]
makeSem_ = genFreer False

------------------------------------------------------------------------------
-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Sem' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer should_mk_sigs type_name = do
  dt_info       <- reifyDatatype type_name
  cl_infos      <- traverse (mkCLInfo dt_info) $ datatypeCons dt_info
  tyfams_ext_on <- isExtEnabled TypeFamilies
  def_mod_fi    <- sequence [ TySynInstD ''DefiningModule
                                . TySynEqn [ConT $ datatypeName dt_info]
                                . LitT
                                . StrTyLit
                                . loc_module
                              <$> location
                            | tyfams_ext_on
                            ]
  sigs          <- if should_mk_sigs
                      then traverse genSig cl_infos
                      else return []
  decs          <- traverse genDec cl_infos

  return $ join $ def_mod_fi : sigs : decs

------------------------------------------------------------------------------
-- | Generates signature for lifting function and type arguments to apply in
-- it's body on effect's data constructor.
genSig :: ConLiftInfo -> Q Dec
genSig
  (CLInfo
    { cliTypeName
    , cliFunName
    , cliEffTArgs
    , cliMName
    , cliResArg
    , cliConCxt
    , cliConArgs
    })
  = do
  r <- newName "r"
  let member_cxt = ConT ''Member `AppT` eff `AppT` VarT r
      eff        = foldl' AppT (ConT cliTypeName) cliEffTArgs
      -- TODO: can use of 'm' blow up somehow?
      f_args     = replaceMArg cliMName r cliConArgs
      return_t   = ConT ''Sem `AppT` VarT r `AppT` cliResArg

  return $ SigD cliFunName $ quantifyType $
    ForallT [] (member_cxt : cliConCxt) $ foldArrows $ f_args ++ [return_t]

------------------------------------------------------------------------------
-- | Replaces use of @m@ in type with @Sem r@.
replaceMArg :: TypeSubstitution t => Name -> Name -> t -> t
replaceMArg m r = applySubstitution $ M.singleton m $ ConT ''Sem `AppT` VarT r

------------------------------------------------------------------------------
-- | Builds a function definition of the form @x a b c = send $ X a b c@.
genDec :: ConLiftInfo -> Q [Dec]
-- TODO: apply effect's type arguments to avoid "bad ambiguity"
genDec (CLInfo { cliFunName, cliConName, cliConArgs }) = do
  f_args_names <- replicateM (length cliConArgs) $ newName "x"

  return
    [ PragmaD $ InlineP cliFunName Inlinable ConLike AllPhases
    , FunD cliFunName
        [ Clause (VarP <$> f_args_names)
                 (NormalB $ AppE (VarE 'send) $
                   foldl1' AppE $ ConE cliConName : (VarE <$> f_args_names))
                 []
        ]
    ]

-------------------------------------------------------------------------------
-- | Info about constructor being lifted; use 'mkCLInfo' to create one.
data ConLiftInfo = CLInfo
  { cliTypeName :: Name    -- ^ type constructor of effect
  , cliFunName  :: Name    -- ^ final lifting function
  , cliConName  :: Name    -- ^ data constructor being lifted
  , cliEffTArgs :: [Type]  -- ^ effect specific arguments to type constructor
  , cliMName    :: Name    -- ^ monad argument in effect
  , cliResArg   :: Type    -- ^ result argument in effect
  , cliConCxt   :: Cxt     -- ^ constraints introduced by constructor
  , cliConArgs  :: [Type]  -- ^ constructor's arguments
  } deriving Show

------------------------------------------------------------------------------
-- | Creates info about constructor being lifted from parent type's and it's
-- own info.
mkCLInfo :: DatatypeInfo -> ConstructorInfo -> Q ConLiftInfo
mkCLInfo
  (DatatypeInfo
    { datatypeName = cliTypeName
    , datatypeVars
    , datatypeInstTypes
    })
  (ConstructorInfo
    { constructorName = cliConName
    , constructorContext
    , constructorFields
    })
  = do
  let cliFunName            = mapNameBase (mapHead toLower) cliConName
      all_eff_t_args        = normalizeType <$> datatypeInstTypes
      cliConArgs            = normalizeType <$> constructorFields
      normalizeType         = removeTypeKind . applySubstitution eq_pairs
      -- We extract equality constraints with variables to unify them
      -- manually - this makes type errors more readable. Plus we replace
      -- kind of result with 'Type' if it is a type variable.
      (eq_pairs, cliConCxt) = first (M.fromList . maybeAddResKind)
                            $ partitionEithers
                            $ eqPairOrCxt <$> constructorContext
      -- TODO: we probably want to add requirement for KindSignatures into
      -- documentation - PolyKinds are already mentioned...
      -- TODO: take result type by name or position?
      maybeAddResKind       = maybe id (\k ps -> (k, StarT) : ps)
                            $ tVarName $ tvKind $ last datatypeVars

  (cliEffTArgs, [m_arg, cliResArg]) <-
    case splitFromEnd 2 all_eff_t_args of
      r@(_, [_, _]) -> return r
      _             -> fail $ show $ missingEffTArgs cliTypeName

                    -- May happen when used on data families
  cliMName <- maybe (fail $ show $ mArgNotVar cliTypeName m_arg)
                    return
                    (tVarName m_arg)

  return CLInfo { cliTypeName
                , cliFunName
                , cliConName
                , cliEffTArgs
                , cliMName
                , cliResArg
                , cliConCxt
                , cliConArgs
                }

------------------------------------------------------------------------------
-- Error messages

mArgNotVar :: Name -> Type -> Doc
mArgNotVar name mArg
  =  text "Monad argument ‘" <> ppr mArg <> text "’ in effect ‘"
  <> ppr name <> text "’ is not a type variable"

missingEffTArgs :: Name -> Doc
missingEffTArgs name
  | base <- mapNameBase id name
  , args <- PlainTV . mkName <$> ["m", "a"]

  =   text "Effect ‘" <> ppr name
      <> text "’ has not enough type arguments"
  $+$ nest 4
      (   text "At least monad and result argument are required, e.g.:"
      $+$ nest 4
          (   text ""
          $+$ ppr (DataD [] base args Nothing [] []) <+> text "..."
          $+$ text ""
          )
      )

------------------------------------------------------------------------------
-- Removes 'Type' kind signatures from type
removeTypeKind :: Type -> Type
removeTypeKind = everywhere $ mkT $ \case
  SigT t StarT    -> t
  ForallT bs cs t -> ForallT (goBndr <$> bs) cs t
    where
      goBndr (KindedTV n StarT) = PlainTV n
      goBndr b                  = b
  t               -> t

------------------------------------------------------------------------------
-- | Converts equality constraint with type variable to name and type pair or
-- leaves predicate as is
eqPairOrCxt :: Pred -> Either (Name, Type) Pred
eqPairOrCxt p = case asEqualPred p of
  Just (VarT n, b) -> Left (n, b)
  Just (a, VarT n) -> Left (n, a)
  _                -> Right p

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
-- | Constructs name from base of input name with function applied
mapNameBase :: (String -> String) -> Name -> Name
mapNameBase f = mkName . f . nameBase

------------------------------------------------------------------------------
-- TODO: separate into some module for general utilities (or find them in
-- available dependencies)

splitFromEnd :: Int -> [a] -> ([a], [a])
splitFromEnd n lst = go lst $ drop n lst
  where
    go xs     []     = ([], xs)
    go (x:xs) (_:ys) = first (x:) $ go xs ys
    go []     (_:_)  = error
      "splitFromEnd: list with dropped elements is longer"

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs
