{-# OPTIONS_HADDOCK not-home #-}

{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE MonadComprehensions #-}
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
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.PprLib
import Language.Haskell.TH.Datatype
import Polysemy.Internal              (send, Member, Sem)
import Polysemy.Internal.CustomErrors (DefiningModule)

import qualified Data.Map.Strict as M

-- TODO: update documentation based on other decisions

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
genFreer shouldMkSigs typeName = do
  dtInfo <- reifyDatatype typeName
  -- TODO: data families seem to work just fine - should they be restricted?
  clInfos  <- traverse (mkCLInfo dtInfo) $ datatypeCons dtInfo
  defModFI <- TySynInstD ''DefiningModule
          .   TySynEqn [ConT $ datatypeName dtInfo]
          .   LitT
          .   StrTyLit
          .   loc_module
          <$> location
  sigs     <- if shouldMkSigs then traverse genSig clInfos
                              else return []
  decs     <- traverse genDec clInfos

  return $ defModFI : join (sigs:decs)

-------------------------------------------------------------------------------
-- | Info about constructor being lifted; use 'mkCLInfo' to create one.
data ConLiftInfo = CLInfo {
    typeName :: Name
  , funName  :: Name
  , conName  :: Name
  , effTArgs :: [Type]
  , mName    :: Name
  , resArg   :: Type
  , conCxt   :: Cxt
  , conArgs  :: [Type]
  } deriving Show

------------------------------------------------------------------------------
-- | Creates info about constructor being lifted from parent type's and it's
-- own info.
mkCLInfo :: DatatypeInfo -> ConstructorInfo -> Q ConLiftInfo
-- mkCLInfo dtInfo conInfo = do
mkCLInfo DatatypeInfo {
             datatypeName = typeName
           , datatypeVars
           , datatypeInstTypes
           }
         ConstructorInfo {
             constructorName = conName
           , constructorContext
           , constructorFields
           }
  = do
  let funName           = mapNameBase (mapHead toLower) conName
      -- We extract equality constraints with variables to unify them
      -- manually - this makes type errors more readable
      (eqPairs, conCxt) = first (M.fromList . maybeAddResKind)
                        $ partitionWith eqPairOrCxt constructorContext
      -- We constrain kind of result to 'Type' if we get a type variable
      -- TODO: we probably want to add requirement for KindSignatures into
      -- documentation - PolyKinds are already mentioned...
      -- TODO: This makes function signatures ugly - what is the best way to
      -- restrict occurence of kind signature or completely remove it?
      -- TODO: take result type by name or position?
      maybeAddResKind   = maybe id ((:) . (, StarT))
                        $ tvName' $ tvKind $ last datatypeVars
      allEffTArgs       = applySubstitution eqPairs datatypeInstTypes
      conArgs           = applySubstitution eqPairs constructorFields

  (effTArgs, [mArg, resArg]) <- case splitFromEnd 2 allEffTArgs of
      r@(_, [_, _]) -> return r
      _             -> fail $ show $ missingEffTArgs typeName
  mName <-
    -- May happen when used on data families
    maybe (fail $ show $ mArgNotVar typeName mArg) return $ tvName' mArg

  return CLInfo {
    typeName, funName, conName, effTArgs, mName, resArg, conCxt, conArgs
    }
  where
    mArgNotVar name mArg
      =   text "Monad argument ‘" <> ppr mArg <> text "’ in effect ‘"
          <> ppr name <> text "’ is not a type variable"

    missingEffTArgs name
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
      where
        base = mapNameBase id name
        args = PlainTV . mkName <$> ["m", "a"]

------------------------------------------------------------------------------
-- | Generates signature for lifting function and type arguments to apply in
-- it's body on effect's data constructor.
genSig :: ConLiftInfo -> Q Dec
genSig CLInfo {
    typeName, funName, effTArgs, mName, resArg, conCxt, conArgs
  } = do
  r <- newName "r"
  let memberCxt  = ConT ''Member `AppT` eff `AppT` VarT r
      eff        = foldl' AppT (ConT typeName) effTArgs
      -- TODO: can use of 'm' blow up somehow?
      funArgs    = replaceMArg mName r conArgs
      returnType = ConT ''Sem `AppT` VarT r `AppT` resArg

  return $ SigD funName $ quantifyType $
    ForallT [] (memberCxt : conCxt) $ foldArrows $ funArgs ++ [returnType]

  where
    -- Replaces use of @m@ in type with @Sem r@.
    replaceMArg m r = applySubstitution
                    $ M.singleton m (ConT ''Sem `AppT` VarT r)

------------------------------------------------------------------------------
-- | Builds a function definition of the form @x a b c = send $ X a b c@.
genDec :: ConLiftInfo -> Q [Dec]
-- TODO: apply effect's type arguments to avoid "bad ambiguity"
genDec CLInfo { funName, conName, conArgs } = do
  funArgsNames <- replicateM (length conArgs) $ newName "x"

  return [
      PragmaD $ InlineP funName Inlinable ConLike AllPhases
    , FunD funName [
        Clause (VarP <$> funArgsNames)
               (NormalB $ AppE (VarE 'send) $
                 foldl1 AppE $ ConE conName : fmap VarE funArgsNames)
               []
      ]
    ]

------------------------------------------------------------------------------
-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 $ AppT . AppT ArrowT

------------------------------------------------------------------------------
-- | Extracts name from type variable (possibly nested in signature and/or
-- some context), returns 'Nothing' otherwise.
tvName' :: Type -> Maybe Name
tvName' = \case
  ForallT _ _ t -> tvName' t
  SigT t _      -> tvName' t
  VarT n        -> Just n
  ParensT t     -> tvName' t
  _             -> Nothing

------------------------------------------------------------------------------
-- | Converts equality constraint with type variable to name and type pair or
-- leaves predicate as is
eqPairOrCxt :: Pred -> Either (Name, Type) Pred
eqPairOrCxt p = case asEqualPred p of
  Just (VarT n, b) -> Left (n, b)
  Just (a, VarT n) -> Left (n, a)
  _                -> Right p

------------------------------------------------------------------------------
-- | Constructs name from base of input name with function applied
mapNameBase :: (String -> String) -> Name -> Name
mapNameBase f = mkName . f . nameBase

------------------------------------------------------------------------------
-- TODO: separate into some module for general utilities (or find them in
-- available dependencies)

partitionWith :: (a -> Either b c) -> [a] -> ([b], [c])
partitionWith f = foldl' choose ([], []) where
  choose = flip $ either (first . (:)) (second . (:)) . f

splitFromEnd :: Int -> [a] -> ([a], [a])
splitFromEnd n lst = go lst $ drop n lst where
  go xs     []     = ([], xs)
  go (x:xs) (_:ys) = first (x:) $ go xs ys

  go []     (_:_)  = undefined

mapHead :: (a -> a) -> [a] -> [a]
mapHead _ []     = []
mapHead f (x:xs) = f x : xs
