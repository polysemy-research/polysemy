{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TupleSections     #-}

-- Originally ported from code written by Sandy Maguire (@isovector), available
-- at https://github.com/IxpertaSolutions/freer-effects/pull/28.

{-|
This module provides Template Haskell functions for automatically generating
effect operation functions (that is, functions that use 'send') from a given
effect algebra. For example, using the @FileSystem@ effect from the example in
the module documentation for "Polysemy", we can write the following:

@
data FileSystem r where
  ReadFile :: 'FilePath' -> FileSystem 'String'
  WriteFile :: 'FilePath' -> 'String' -> FileSystem ()
'makeEffect' ''FileSystem
@

This will automatically generate the following functions:

@
readFile :: 'Member' FileSystem effs => 'FilePath' -> 'Eff' effs 'String'
readFile a = 'send' (ReadFile a)

writeFile :: 'Member' FileSystem effs => 'FilePath' -> 'String' -> 'Eff' effs ()
writeFile a b = 'send' (WriteFile a b)
@
-}
module Polysemy.Internal.TH.Effect
  ( makeSemantic
  , makeSemantic_
  )
where

import Control.Monad (forM, unless)
import Data.Char (toLower)
import Data.List
import Generics.SYB
import Language.Haskell.TH
import Polysemy (send, Member, Semantic)
import Prelude
import Polysemy.Union.TypeErrors (DefiningModule)


-- | If @T@ is a GADT representing an effect algebra, as described in the module
-- documentation for "Polysemy", @$('makeEffect' ''T)@ automatically
-- generates a function that uses 'send' with each operation. For more
-- information, see the module documentation for "Polysemy.TH".
makeSemantic :: Name -> Q [Dec]
makeSemantic = genFreer True

-- | Like 'makeEffect', but does not provide type signatures. This can be used
-- to attach Haddock comments to individual arguments for each generated
-- function.
--
-- @
-- data Lang x where
--   Output :: String -> Lang ()
--
-- makeSemantic_ ''Lang
--
-- -- | Output a string.
-- output :: Member Lang effs
--        => String    -- ^ String to output.
--        -> Semantic effs ()  -- ^ No result.
-- @
--
-- Note that 'makeEffect_' must be used /before/ the explicit type signatures.
makeSemantic_ :: Name -> Q [Dec]
makeSemantic_ = genFreer False

-- | Generates declarations and possibly signatures for functions to lift GADT
-- constructors into 'Eff' actions.
genFreer :: Bool -> Name -> Q [Dec]
genFreer makeSigs tcName = do
  -- The signatures for the generated definitions require FlexibleContexts.
  isExtEnabled FlexibleContexts
    >>= flip unless (fail "makeSemantic requires FlexibleContexts to be enabled")
  hasTyFams <- isExtEnabled TypeFamilies

  reify tcName >>= \case
    TyConI (DataD _ _ _ _ cons _) -> do
      sigs <- filter (const makeSigs) <$> mapM genSig cons
      decs <- mapM genDecl cons
      loc <- location

      return $
        [ TySynInstD ''DefiningModule
            . TySynEqn [ConT tcName]
            . LitT
            . StrTyLit
            $ loc_module loc
        | hasTyFams
        ] ++ sigs ++ decs

    _ -> fail "makeSemantic expects a type constructor"

-- | Given the name of a GADT constructor, return the name of the corresponding
-- lifted function.
getDeclName :: Name -> Name
getDeclName = mkName . overFirst toLower . nameBase
 where
  overFirst f (a : as) = f a : as
  overFirst _ as       = as

-- | Builds a function definition of the form @x a b c = send $ X a b c@.
genDecl :: Con -> Q Dec
genDecl (ForallC _       _     con) = genDecl con
genDecl (GadtC   [cName] tArgs _  ) = do
  let fnName = getDeclName cName
  let arity  = length tArgs - 1
  dTypeVars <- forM [0 .. arity] $ const $ newName "a"
  return $ FunD fnName . pure $ Clause
    (VarP <$> dTypeVars)
    (NormalB . AppE (VarE 'send) $ foldl
      (\b -> AppE b . VarE)
      (ConE cName)
      dTypeVars
    )
    []
genDecl _ = fail "genDecl expects a GADT constructor"

tyVarBndrName :: TyVarBndr -> Name
tyVarBndrName (PlainTV n) = n
tyVarBndrName (KindedTV n _) = n

tyVarBndrKind :: TyVarBndr -> Maybe Type
tyVarBndrKind (PlainTV _) = Nothing
tyVarBndrKind (KindedTV _ k) = Just k

-- | Generates a function type from the corresponding GADT type constructor
-- @x :: Member (Effect e) effs => a -> b -> c -> Semantic effs r@.
genType :: Con -> Q (Type, Maybe Name, Maybe Type)
genType (ForallC tyVarBindings conCtx con) = do
  (t, mn, _) <- genType con
  let k = do n <- mn
             z <- find ((== n) . tyVarBndrName) tyVarBindings
             tyVarBndrKind z
      free = everything mappend freeVars t
  pure ( ForallT (filter (flip elem free . tyVarBndrName) tyVarBindings) conCtx t
       , mn
       , k
       )
genType (GadtC   _ tArgs' (eff `AppT` m `AppT` tRet)) = do
  effs <- newName "effs"
  let
    tArgs            = fmap snd tArgs'
    memberConstraint = ConT ''Member `AppT` eff `AppT` VarT effs
    resultType       = ConT ''Semantic `AppT` VarT effs `AppT` tRet

    replaceMType t | t == m = ConT ''Semantic `AppT` VarT effs
                   | otherwise = t
    ts = everywhere (mkT replaceMType) tArgs
    tn = case tRet of
           VarT n -> Just n
           _ -> Nothing

  pure
    . (, tn, Nothing)
    .  ForallT [PlainTV effs] [memberConstraint]
    .  foldArrows
    $  ts
    ++ [resultType]
-- TODO: Although this should never happen, we obviously need a better error message below.
genType _       = fail "genSig expects a GADT constructor"

-- | Turn all (KindedTV tv StarT) into (PlainTV tv) in the given type
-- This can prevent the need for KindSignatures
simplifyBndrs :: Maybe Type -> Type -> Type
simplifyBndrs star = everywhere (mkT $ simplifyBndr star)

-- | Turn TvVarBndrs of the form (KindedTV tv StarT) into (PlainTV tv)
-- This can prevent the need for KindSignatures
simplifyBndr :: Maybe Type -> TyVarBndr -> TyVarBndr
simplifyBndr (Just star) (KindedTV tv k) | star == k = PlainTV tv
simplifyBndr _ (KindedTV tv StarT) = PlainTV tv
simplifyBndr _ bndr = bndr

-- | Generates a type signature of the form
-- @x :: Member (Effect e) effs => a -> b -> c -> Semantic effs r@.
genSig :: Con -> Q Dec
genSig con = do
  let
    getConName (ForallC _ _ c) = getConName c
    getConName (GadtC [n] _ _) = pure n
    getConName c = fail $ "failed to get GADT name from " ++ show c
  conName <- getConName con
  (t, _, k) <- genType con
  pure $ SigD (getDeclName conName) $ simplifyBndrs k t

-- | Folds a list of 'Type's into a right-associative arrow 'Type'.
foldArrows :: [Type] -> Type
foldArrows = foldr1 (AppT . AppT ArrowT)


freeVars :: Data a => a -> [Name]
freeVars = mkQ [] $ \case
  VarT n -> [n]
  _ -> []

