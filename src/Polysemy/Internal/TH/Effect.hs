{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections   #-}

-- | This module provides Template Haskell functions for automatically generating
-- effect operation functions (that is, functions that use 'send') from a given
-- effect algebra. For example, using the @FileSystem@ effect from the example in
-- the module documentation for "Polysemy", we can write the following:
--
-- @
-- data FileSystem m a where
--   ReadFile  :: 'FilePath' -> FileSystem 'String'
--   WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
--
-- 'makeSem' ''FileSystem
-- @
--
-- This will automatically generate (approximately) the following functions:
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
import Data.Tuple
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Datatype
import Polysemy.Internal              (send, Member, Sem)
import Polysemy.Internal.CustomErrors (DefiningModule)

import qualified Data.Map.Strict as M

-- TODO: write tests for what should (not) compile

------------------------------------------------------------------------------
-- | If @T@ is a GADT representing an effect algebra, as described in the
-- module documentation for "Polysemy", @$('makeSem' ''T)@ automatically
-- generates a smart constructor for every data constructor of @T@. This also
-- works for data family instances.
--
-- @since 0.1.2.0
makeSem :: Name -> Q [Dec]
makeSem = genFreer True

-- | Like 'makeSem', but does not provide type signatures. This can be used
-- to attach Haddock comments to individual arguments for each generated
-- function.
--
-- @
-- data Output o m a where
--   Output :: o -> Output o m ()
--
-- makeSem_ ''Output
--
-- -- | Output the value \@o\@.
-- output :: forall o r
--        .  Member (Output o) r
--        => o         -- ^ Value to output.
--        -> Sem r ()  -- ^ No result.
-- @
--
-- Because of limitations in Template Haskell, signatures have to follow some
-- rules to work properly:
--
-- * 'makeSem_' must be used /before/ the explicit type signatures
-- * signatures have to specify argument of 'Sem' representing union of
-- effects as @r@ (e.g. @'Sem' r ()@)
-- * all arguments in effect's type constructor have to follow naming scheme
-- from effect's declaration:
--
-- @
-- data Foo e m a where
--   FooC1 :: Foo x m ()
--   FooC2 :: Foo (Maybe x) m ()
-- @
--
-- should have @e@ in type signature of @fooC1@:
--
-- @fooC1 :: forall e r. Member (Foo e) r => Sem r ()@
--
-- but @x@ in signature of @fooC2@:
--
-- @fooC2 :: forall x r. Member (Foo (Maybe x)) r => Sem r ()@
--
-- * all effect's type variables and @r@ have to be explicitly quantified
-- using @forall@ (order is not important)
--
-- These restrictions may be removed in the future, depending on changes to
-- the compiler.
--
-- @since 0.1.2.0
makeSem_ :: Name -> Q [Dec]
makeSem_ = genFreer False
-- NOTE(makeSem_):
-- This function uses an ugly hack to work --- it enables change of names in
-- annotation of applied data constructor to capturable ones, based of names
-- in effect's definition. This allows user to provide them to us from their
-- signature through 'forall' with 'ScopedTypeVariables' enabled, so that we
-- can compile liftings of constructors with ambiguous type arguments (see
-- issue #48).
--
-- Please, change this as soon as GHC provides some way of inspecting
-- signatures, replacing code or generating haddock documentation in TH.

------------------------------------------------------------------------------
-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Sem' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer should_mk_sigs type_name = do
  checkExtensions [ScopedTypeVariables, FlexibleContexts]
  dt_info    <- reifyDatatype type_name
  cl_infos   <- traverse (mkCLInfo dt_info) $ datatypeCons dt_info
  tyfams_on  <- isExtEnabled TypeFamilies
  def_mod_fi <- sequence [ TySynInstD ''DefiningModule
                             . TySynEqn [ConT $ datatypeName dt_info]
                             . LitT
                             . StrTyLit
                             . loc_module
                           <$> location
                         | tyfams_on
                         ]
  decs       <- traverse (genDec should_mk_sigs) cl_infos

  let sigs = [ genSig <$> cl_infos | should_mk_sigs ]

  return $ join $ def_mod_fi : sigs ++ decs

------------------------------------------------------------------------------
-- | Generates signature for lifting function and type arguments to apply in
-- its body on effect's data constructor.
genSig :: ConLiftInfo -> Dec
genSig cli
  = SigD (cliFunName cli) $ quantifyType
  $ ForallT [] (member_cxt : cliFunCxt cli)
  $ foldArrows $ cliFunArgs cli ++ [sem `AppT` cliResType cli]
  where
    member_cxt = classPred ''Member [eff, VarT $ cliUnionName cli]
    eff        = foldl' AppT (ConT $ cliEffName cli) $ cliEffArgs cli
    sem        = ConT ''Sem `AppT` VarT (cliUnionName cli)

------------------------------------------------------------------------------
-- | Builds a function definition of the form
-- @x a b c = send (X a b c :: E m a)@.
genDec :: Bool -> ConLiftInfo -> Q [Dec]
genDec should_mk_sigs cli = do
  fun_args_names <- replicateM (length $ cliFunArgs cli) $ newName "x"

  let action = foldl1' AppE
             $ ConE (cliConName cli) : (VarE <$> fun_args_names)
      eff    = foldl' AppT (ConT $ cliEffName cli) $ args
               -- see NOTE(makeSem_)
      args   = (if should_mk_sigs then id else map capturableTVars)
             $ cliEffArgs cli ++ [sem, cliResType cli]
      sem    = ConT ''Sem `AppT` VarT (cliUnionName cli)

  return
    [ PragmaD $ InlineP (cliFunName cli) Inlinable ConLike AllPhases
    , FunD (cliFunName cli)
        [ Clause (VarP <$> fun_args_names)
                 (NormalB $ AppE (VarE 'send) $ SigE action eff)
                 []
        ]
    ]

-------------------------------------------------------------------------------
-- | Info about constructor being lifted; use 'mkCLInfo' to create one.
data ConLiftInfo = CLInfo
  { cliEffName   :: Name    -- ^ name of effect's type constructor
  , cliEffArgs   :: [Type]  -- ^ effect specific type arguments
  , cliResType   :: Type    -- ^ result type specific to action
  , cliConName   :: Name    -- ^ name of action constructor
  , cliFunName   :: Name    -- ^ name of final function
  , cliFunArgs   :: [Type]  -- ^ final function arguments
  , cliFunCxt    :: Cxt     -- ^ constraints of final function
  , cliUnionName :: Name    -- ^ name of type variable parameterizing 'Sem'
  } deriving Show

------------------------------------------------------------------------------
-- | Creates info about smart constructor being created from info about action
-- and it's parent type.
mkCLInfo :: DatatypeInfo -> ConstructorInfo -> Q ConLiftInfo
mkCLInfo dti ci = do
  let cliEffName            = datatypeName dti

  (raw_cli_eff_args, [m_arg, raw_cli_res_arg]) <-
    case splitAtEnd 2 $ datatypeInstTypes dti of
      r@(_, [_, _]) -> return r
      _             -> missingEffTArgs cliEffName

  m_name <-
    case tVarName m_arg of
      Just r        -> return r
      Nothing       -> mArgNotVar cliEffName m_arg

  cliUnionName <- newName "r"

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
      cliFunName            = mkName $ mapHead toLower $ nameBase
                            $ constructorName ci
      cliFunArgs            = normalizeType <$> constructorFields ci

  return CLInfo{..}

------------------------------------------------------------------------------
-- Error messages and checks

mArgNotVar :: Name -> Type -> Q a
mArgNotVar name mArg = fail $ show
  $  text "Monad argument ‘" <> ppr mArg <> text "’ in effect ‘"
  <> ppr name <> text "’ is not a type variable"

missingEffTArgs :: Name -> Q a
missingEffTArgs name = fail $ show
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
    base = mkName $ nameBase $ name
    args = PlainTV . mkName <$> ["m", "a"]

checkExtensions :: [Extension] -> Q ()
checkExtensions exts = do
  states <- zip exts <$> traverse isExtEnabled exts
  maybe (return ())
        (\(ext, _) -> fail $ show
          $ char '‘' <> text (show ext) <> char '’'
            <+> text "extension needs to be enabled\
                     \for smart constructors to work")
        (find (not . snd) states)

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
-- | Constructs capturable name from base of input name.
capturableBase :: Name -> Name
capturableBase = mkName . nameBase

------------------------------------------------------------------------------
-- | 'splitAt' counting from the end.
splitAtEnd :: Int -> [a] -> ([a], [a])
splitAtEnd n = swap . join bimap reverse . splitAt n . reverse

------------------------------------------------------------------------------
-- | Applies function to head of list, if there is one.
mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs
