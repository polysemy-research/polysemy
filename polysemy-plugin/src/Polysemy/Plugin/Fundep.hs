{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) 2017 Luka Horvat
--
-- Permission is hereby granted, free of charge, to any person obtaining a copy
-- of this software and associated documentation files (the "Software"), to
-- deal in the Software without restriction, including without limitation the
-- rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
-- sell copies of the Software, and to permit persons to whom the Software is
-- furnished to do so, subject to the following conditions:
--
-- The above copyright notice and this permission notice shall be included in
-- all copies or substantial portions of the Software.
--
-- THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
-- IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
-- FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
-- AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
-- LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
-- FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS
-- IN THE SOFTWARE.
--
------------------------------------------------------------------------------
--
-- This module is heavily based on 'Control.Effects.Plugin' from the
-- 'simple-effects' package, originally by Luka Horvat.
--
-- https://gitlab.com/LukaHorvat/simple-effects/commit/966ce80b8b5777a4bd8f87ffd443f5fa80cc8845#f51c1641c95dfaa4827f641013f8017e8cd02aab

module Polysemy.Plugin.Fundep (fundepPlugin) where

import           Class
import           CoAxiom
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Bool
import           Data.Coerce
import           Data.Function (on)
import           Data.IORef
import qualified Data.Kind as K
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           FastString (fsLit)
import           GHC (TyCon, Name)
import           GHC.TcPluginM.Extra (lookupModule, lookupName)
import           Module (mkModuleName)
import           OccName (mkTcOcc)
import           TcEvidence
import           TcPluginM (TcPluginM, tcLookupClass, tcLookupTyCon, tcPluginIO)
import           TcRnTypes
import           TcSMonad hiding (tcLookupClass)
import           TyCoRep (Type (..))
import           Type


data LookupState
  = Locations
  | Things


type family ThingOf (l :: LookupState) (a :: K.Type) :: K.Type where
  ThingOf 'Locations _ = (String, String)
  ThingOf 'Things a = a


data PolysemyStuff (l :: LookupState) = PolysemyStuff
  { findClass    :: ThingOf l Class
  , semTyCon     :: ThingOf l TyCon
  , ifStuckTyCon :: ThingOf l TyCon
  , indexOfTyCon :: ThingOf l TyCon
  }


class CanLookup a where
  lookupStrategy :: Name -> TcPluginM a

instance CanLookup Class where
  lookupStrategy = tcLookupClass

instance CanLookup TyCon where
  lookupStrategy = tcLookupTyCon


doLookup :: CanLookup a => ThingOf 'Locations a -> TcPluginM (ThingOf 'Things a)
doLookup (mdname, name) = do
  md  <- lookupModule (mkModuleName mdname) $ fsLit "polysemy"
  nm <- lookupName md $ mkTcOcc name
  lookupStrategy nm


lookupEverything :: PolysemyStuff 'Locations -> TcPluginM (PolysemyStuff 'Things)
lookupEverything (PolysemyStuff a b c d) =
  PolysemyStuff <$> doLookup a
                <*> doLookup b
                <*> doLookup c
                <*> doLookup d


polysemyStuffLocations :: PolysemyStuff 'Locations
polysemyStuffLocations = PolysemyStuff
  { findClass    = ("Polysemy.Internal.Union",                  "Find")
  , semTyCon     = ("Polysemy.Internal",                        "Sem")
  , ifStuckTyCon = ("Polysemy.Internal.CustomErrors.Redefined", "IfStuck")
  , indexOfTyCon = ("Polysemy.Internal.Union",                  "IndexOf")
  }


fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
    { tcPluginInit =
        (,) <$> tcPluginIO (newIORef S.empty)
            <*> lookupEverything polysemyStuffLocations
    , tcPluginSolve = solveFundep
    , tcPluginStop = const (return ()) }

allMonadEffectConstraints :: PolysemyStuff 'Things -> [Ct] -> [(CtLoc, (Type, Type, Type))]
allMonadEffectConstraints (findClass -> cls) cts =
    [ (ctLoc cd, (effName, eff, r))
    | cd@CDictCan{cc_class = cls', cc_tyargs = [_, r, eff]} <- cts
    , cls == cls'
    , let effName = getEffName eff
    ]

singleListToJust :: [a] -> Maybe a
singleListToJust [a] = Just a
singleListToJust _ = Nothing

findMatchingEffectIfSingular :: (Type, Type, Type) -> [(Type, Type, Type)] -> Maybe Type
findMatchingEffectIfSingular (effName, _, mon) ts = singleListToJust
    [ eff'
        | (effName', eff', mon') <- ts
        , eqType effName effName'
        , eqType mon mon' ]

getEffName :: Type -> Type
getEffName t = fst $ splitAppTys t


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


------------------------------------------------------------------------------
-- | Like 'Control.Monad.when', but in the context of an 'Alternative'.
whenA
    :: (Monad m, Alternative z)
    => Bool
    -> m a
    -> m (z a)
whenA False _ = pure empty
whenA True ma = fmap pure ma


mkWanted
    :: SolveContext
    -> CtLoc
    -> Type
    -> Type
    -> TcPluginM (Maybe ( (OrdType, OrdType)  -- the types we want to unify
                        , Ct                  -- the constraint
                        ))
mkWanted solve_ctx loc wanted given =
  whenA (not (mustUnify solve_ctx) || canUnifyRecursive solve_ctx wanted given) $ do
    (ev, _) <- unsafeTcPluginTcM
             . runTcSDeriveds
             $ newWantedEq loc Nominal wanted given
    pure ( (OrdType wanted, OrdType given)
         , CNonCanonical ev
         )

thd :: (a, b, c) -> c
thd (_, _, c) = c

countLength :: (a -> a -> Bool) -> [a] -> [(a, Int)]
countLength eq as =
  let grouped = groupBy eq as
   in zipWith (curry $ bimap head length) grouped grouped


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


------------------------------------------------------------------------------
-- | Given a list of 'Ct's, find any that are of the form
-- @[Irred] Sem r a ~ Something@, and return their @r@s.
getBogusRs :: PolysemyStuff 'Things -> [Ct] -> [Type]
getBogusRs stuff wanteds = do
  CIrredCan ct _ <- wanteds
  case splitAppTys $ ctev_pred ct of
    (_, [_, _, a, b]) ->
      maybeToList (getRIfSem stuff a) ++ maybeToList (getRIfSem stuff b)
    (_, _) -> []


------------------------------------------------------------------------------
-- | Take the @r@ out of @Sem r a@.
getRIfSem :: PolysemyStuff 'Things -> Type -> Maybe Type
getRIfSem (semTyCon -> sem) ty =
  case splitTyConApp_maybe ty of
    Just (tycon, [r, _]) | tycon == sem -> pure r
    _                                   -> Nothing


------------------------------------------------------------------------------
-- | Given a list of bogus @r@s, and the wanted constraints, produce bogus
-- evidence terms that will prevent @IfStuck (IndexOf r _) _ _@ error messsages.
solveBogusError :: PolysemyStuff 'Things -> [Type] -> [Ct] -> [(EvTerm, Ct)]
solveBogusError stuff bogus wanteds = do
  ct@(CIrredCan ce _) <- wanteds
  case splitTyConApp_maybe $ ctev_pred ce of
    Just (stuck, [_, _, expr, _, _]) | stuck == ifStuckTyCon stuff -> do
      case splitTyConApp_maybe expr of
        Just (idx, [_, r, _]) | idx == indexOfTyCon stuff -> do
          case elem @[] (OrdType r) $ coerce bogus of
            True -> pure (error "bogus proof for stuck type family", ct)
            False -> []
        _ -> []
    _ -> []


solveFundep
    :: (IORef (S.Set (OrdType, OrdType)), PolysemyStuff 'Things)
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
solveFundep (ref, stuff) giv _ want = do
    let bogus = getBogusRs stuff want
        solved_bogus = solveBogusError stuff bogus want

    let wantedEffs = allMonadEffectConstraints stuff want
        givenEffs = snd <$> allMonadEffectConstraints stuff giv
        num_wanteds_by_r = countLength eqType $ fmap (thd . snd) wantedEffs
        must_unify r =
          let Just num_wanted = find (eqType r . fst) num_wanteds_by_r
           in snd num_wanted /= 1

    eqs <- forM wantedEffs $ \(loc, e@(_, eff, r)) -> do
      case findMatchingEffectIfSingular e givenEffs of
        Nothing -> do
          case splitAppTys r of
            (_, [_, eff', _]) -> mkWanted (InterpreterUse $ must_unify r) loc eff eff'
            _                 -> pure Nothing
        Just eff' -> mkWanted FunctionDef loc eff eff'

    already_emitted <- tcPluginIO $ readIORef ref
    let new_wanteds = filter (not . flip S.member already_emitted . fst)
                    $ catMaybes eqs

    tcPluginIO $ modifyIORef ref $ S.union $ S.fromList $ fmap fst new_wanteds
    pure . TcPluginOk solved_bogus $ fmap snd new_wanteds

