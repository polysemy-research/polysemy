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

import           CoAxiom
import           Control.Applicative
import           Control.Monad
import           Data.Bifunctor
import           Data.Bool
import           Data.Coerce
import           Data.Function (on)
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import qualified Data.Map as M
import           Polysemy.Plugin.Fundep.Stuff
import           TcEvidence
import           TcPluginM (TcPluginM, tcPluginIO)
import           TcRnTypes
import           TcSMonad hiding (tcLookupClass)
import           TyCoRep (Type (..))
import           Type



fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
    { tcPluginInit =
        (,) <$> tcPluginIO (newIORef S.empty)
            <*> polysemyStuff
    , tcPluginSolve = solveFundep
    , tcPluginStop = const (return ()) }

data FindConstraint = FindConstraint
  { fcLoc        :: CtLoc
  , fcEffectName :: Type
  , fcEffect     :: Type
  , fcRow        :: Type
  }

getFindConstraints :: PolysemyStuff 'Things -> [Ct] -> [FindConstraint]
getFindConstraints (findClass -> cls) cts =
    [ FindConstraint
        { fcLoc = ctLoc cd
        , fcEffectName = effName
        , fcEffect = eff
        , fcRow = r
        }
    | cd@CDictCan{cc_class = cls', cc_tyargs = [_, r, eff]} <- cts
    , cls == cls'
    , let effName = getEffName eff
    ]

singleListToJust :: [a] -> Maybe a
singleListToJust [a] = Just a
singleListToJust _ = Nothing

findMatchingEffectIfSingular :: FindConstraint -> [FindConstraint] -> Maybe Type
findMatchingEffectIfSingular (FindConstraint _ effName _ mon) ts = singleListToJust
    [ eff'
        | FindConstraint _ effName' eff' mon' <- ts
        , eqType effName effName'
        , eqType mon mon'
        ]

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

data Unification = Unification
  { _unifyLHS :: OrdType
  , _unifyRHS :: OrdType
  }
  deriving (Eq, Ord)


mkWanted
    :: SolveContext
    -> CtLoc
    -> Type
    -> Type
    -> TcPluginM (Maybe (Unification, Ct))
mkWanted solve_ctx loc wanted given =
  whenA (not (mustUnify solve_ctx) || canUnifyRecursive solve_ctx wanted given) $ do
    (ev, _) <- unsafeTcPluginTcM
             . runTcSDeriveds
             $ newWantedEq loc Nominal wanted given
    pure ( Unification (OrdType wanted) (OrdType given)
         , CNonCanonical ev
         )


countLength ::  Eq a => [a] -> [(a, Int)]
countLength as =
  let grouped = group as
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
  (_, [_, _, a, b]) <- pure . splitAppTys $ ctev_pred ct
  maybeToList (getRIfSem stuff a) ++ maybeToList (getRIfSem stuff b)


------------------------------------------------------------------------------
-- | Take the @r@ out of @Sem r a@.
getRIfSem :: PolysemyStuff 'Things -> Type -> Maybe Type
getRIfSem (semTyCon -> sem) ty = do
  (tycon, [r, _]) <- splitTyConApp_maybe ty
  guard $ tycon == sem
  pure r


------------------------------------------------------------------------------
-- | Given a list of bogus @r@s, and the wanted constraints, produce bogus
-- evidence terms that will prevent @IfStuck (IndexOf r _) _ _@ error messsages.
solveBogusError :: PolysemyStuff 'Things -> [Type] -> [Ct] -> [(EvTerm, Ct)]
solveBogusError stuff bogus wanteds = do
  let splitTyConApp_list = maybeToList  . splitTyConApp_maybe

  ct@(CIrredCan ce _) <- wanteds
  (stuck, [_, _, expr, _, _]) <- splitTyConApp_list $ ctev_pred ce
  guard $ stuck == ifStuckTyCon stuff
  (idx, [_, r, _]) <- splitTyConApp_list expr
  guard $ idx == indexOfTyCon stuff
  guard $ elem @[] (OrdType r) $ coerce bogus
  pure (error "bogus proof for stuck type family", ct)


unzipNewWanteds :: S.Set Unification -> [(Unification, Ct)] -> ([Unification], [Ct])
unzipNewWanteds old = unzip . filter (not . flip S.member old . fst)


mustItUnify :: [FindConstraint] -> Type -> Bool
mustItUnify fcs = fromMaybe False
                . flip M.lookup singular_r
                . OrdType
  where
    singular_r = M.fromList
               . fmap (second (/= 1))
               . countLength
               $ fmap (OrdType . fcRow) fcs




solveFundep
    :: (IORef (S.Set Unification), PolysemyStuff 'Things)
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
solveFundep (ref, stuff) giv _ want = do
    let solved_bogus = solveBogusError stuff (getBogusRs stuff want) want
        wanted_finds = getFindConstraints stuff want
        given_finds  = getFindConstraints stuff giv

    eqs <- forM wanted_finds $ \fc -> do
      let loc = fcLoc fc
          eff = fcEffect fc
          r  = fcRow fc
      case findMatchingEffectIfSingular fc given_finds of
        Nothing -> do
          case splitAppTys r of
            (_, [_, eff', _]) -> mkWanted (InterpreterUse $ mustItUnify wanted_finds r) loc eff eff'
            _                 -> pure Nothing
        Just eff' -> mkWanted FunctionDef loc eff eff'

    already_emitted <- tcPluginIO $ readIORef ref
    let (unifications, new_wanteds) = unzipNewWanteds already_emitted $ catMaybes eqs

    tcPluginIO $ modifyIORef ref $ S.union $ S.fromList unifications
    pure $ TcPluginOk solved_bogus new_wanteds

