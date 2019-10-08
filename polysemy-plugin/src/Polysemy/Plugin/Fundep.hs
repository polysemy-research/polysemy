{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns               #-}

------------------------------------------------------------------------------
-- The MIT License (MIT)
--
-- Copyright (c) 2017 Luka Horvat, 2019 Sandy Maguire
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
-- This module was originally based on 'Control.Effects.Plugin' from the
-- 'simple-effects' package, by Luka Horvat.
--
-- https://gitlab.com/LukaHorvat/simple-effects/commit/966ce80b8b5777a4bd8f87ffd443f5fa80cc8845#f51c1641c95dfaa4827f641013f8017e8cd02aab

module Polysemy.Plugin.Fundep (fundepPlugin) where

import           Control.Applicative (empty)
import           Control.Applicative (liftA2)
import           Control.Arrow ((&&&))
import           Control.Monad
import           CoreSyn
import           Data.Bifunctor
import           Data.Coerce
import           Data.Generics hiding (empty)
import           Data.IORef
import           Data.List (sortBy, find)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Ord (Down (..), comparing)
import qualified Data.Set as S
import           DataCon
import           GHC.TcPluginM.Extra (evByFiat)
import           GhcPlugins hiding (empty)
import           Outputable hiding (empty)
import           Polysemy.Plugin.Fundep.Stuff
import           Polysemy.Plugin.Fundep.Unification
import           Polysemy.Plugin.Fundep.Utils
import           TcEvidence
import           TcPluginM (TcPluginM, tcPluginIO, newGiven)
import           TcRnTypes
import           TcSMonad hiding (tcLookupClass)
import           Type



fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
  { tcPluginInit =
      (,) <$> tcPluginIO (newIORef S.empty)
          <*> polysemyStuff
  , tcPluginSolve = solveFundep
  , tcPluginStop = const $ pure ()
  }


------------------------------------------------------------------------------
-- | Corresponds to a 'Polysemy.Internal.Union.Find' constraint. For example,
-- given @Member (State s) r@, we would get:
data FindConstraint = FindConstraint
  { fcLoc        :: CtLoc
  , fcEffectName :: Type  -- ^ @State@
  , fcEffect     :: Type  -- ^ @State s@
  , fcRow        :: Type  -- ^ @r@
  }


------------------------------------------------------------------------------
-- | Given a list of constraints, filter out the 'FindConstraint's.
getFindConstraints :: PolysemyStuff 'Things -> [Ct] -> [FindConstraint]
getFindConstraints (findClass -> cls) cts = do
  cd@CDictCan{cc_class = cls', cc_tyargs = [_, r, eff]} <- cts
  guard $ cls == cls'
  pure $ FindConstraint
    { fcLoc = ctLoc cd
    , fcEffectName = getEffName eff
    , fcEffect = eff
    , fcRow = r
    }


------------------------------------------------------------------------------
-- | If there's only a single @Member@ in the same @r@ whose effect name
-- matches and could possibly unify, return its effect (including tyvars.)
findMatchingEffectIfSingular
    :: FindConstraint
    -> [FindConstraint]
    -> Maybe Type
findMatchingEffectIfSingular (FindConstraint _ eff_name wanted r) ts =
  singleListToJust $ do
    FindConstraint _ eff_name' eff' r' <- ts
    guard $ eqType eff_name eff_name'
    guard $ eqType r r'
    guard $ canUnifyRecursive FunctionDef wanted eff'
    pure eff'


oneMostSpecific :: [Type] -> Maybe Type
oneMostSpecific tys =
  let sorted = sortBy (comparing $ Down . fst) $ fmap (specificityMetric &&& id) tys
   in case pprTrace "sorted" (ppr sorted) sorted of
        ((p1, t1) : (p2, t2) : _) | p1 /= p2 -> Just t1
        [(_, t1)] ->  Just t1
        _ -> Nothing


------------------------------------------------------------------------------
-- | Given an effect, compute its effect name.
getEffName :: Type -> Type
getEffName t = fst $ splitAppTys t


------------------------------------------------------------------------------
-- | Generate a wanted unification for the effect described by the
-- 'FindConstraint' and the given effect.
mkWantedForce
  :: FindConstraint
  -> Type
  -> TcPluginM Unification
mkWantedForce fc given = do
  ev <- newGiven (fcLoc fc) (mkPrimEqPred wanted given)
      . Coercion
      $ mkTcNomReflCo given
  pure . Unification (OrdType wanted) (OrdType given)
       $ CNonCanonical ev
  where
    wanted = fcEffect fc

------------------------------------------------------------------------------
-- | Generate a wanted unification for the effect described by the
-- 'FindConstraint' and the given effect --- if they can be unified in this
-- context.
mkWanted
    :: FindConstraint
    -> SolveContext
    -> Type  -- ^ The given effect.
    -> TcPluginM (Maybe Unification)
mkWanted fc solve_ctx given =
  whenA (not (mustUnify solve_ctx) || canUnifyRecursive solve_ctx wanted given) $
    mkWantedForce fc given
  where
    wanted = fcEffect fc


------------------------------------------------------------------------------
-- | Given a list of 'Ct's, find any that are of the form
-- @[Irred] Sem r a ~ Something@, and return their @r@s.
getBogusRs :: PolysemyStuff 'Things -> [Ct] -> [Type]
getBogusRs stuff wanteds = do
  CIrredCan ct _ <- wanteds
  (_, [_, _, a, b]) <- pure . splitAppTys $ ctev_pred ct
  maybeToList (extractRowFromSem stuff a)
    ++ maybeToList (extractRowFromSem stuff b)


------------------------------------------------------------------------------
-- | Take the @r@ out of @Sem r a@.
extractRowFromSem :: PolysemyStuff 'Things -> Type -> Maybe Type
extractRowFromSem (semTyCon -> sem) ty = do
  (tycon, [r, _]) <- splitTyConApp_maybe ty
  guard $ tycon == sem
  pure r


------------------------------------------------------------------------------
-- | Given a list of bogus @r@s, and the wanted constraints, produce bogus
-- evidence terms that will prevent @IfStuck (IndexOf r _) _ _@ error messsages.
solveBogusError :: PolysemyStuff 'Things -> [Ct] -> [(EvTerm, Ct)]
solveBogusError stuff wanteds = do
  let splitTyConApp_list = maybeToList  . splitTyConApp_maybe

  let bogus = getBogusRs stuff wanteds
  ct@(CIrredCan ce _) <- wanteds
  (stuck, [_, _, expr, _, _]) <- splitTyConApp_list $ ctev_pred ce
  guard $ stuck == ifStuckTyCon stuff
  (idx, [_, r, _]) <- splitTyConApp_list expr
  guard $ idx == indexOfTyCon stuff
  guard $ elem @[] (OrdType r) $ coerce bogus
  pure (error "bogus proof for stuck type family", ct)


------------------------------------------------------------------------------
-- | Determine if there is exactly one wanted find for the @r@ in question.
exactlyOneWantedForR
    :: [FindConstraint]  -- ^ Wanted finds
    -> Type              -- ^ Effect row
    -> Bool
exactlyOneWantedForR wanteds
    = fromMaybe False
    . flip M.lookup singular_r
    . OrdType
  where
    singular_r = M.fromList
               -- TODO(sandy): Nothing fails if this is just @second (const
               -- True)@. Why not? Incomplete test suite, or doing too much
               -- work?
               . fmap (second (/= 1))
               . countLength
               $ fmap (OrdType . fcRow) wanteds


solveFundep
    :: ( IORef (S.Set Unification)
       , PolysemyStuff 'Things
       )
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
solveFundep (ref, stuff) given _ wanted = do
  let wanted_finds = getFindConstraints stuff wanted
      given_finds  = getFindConstraints stuff given

  eqs <- forM wanted_finds $ \fc -> do
    let r  = fcRow fc
    case findMatchingEffectIfSingular fc given_finds of
      -- We found a real given, therefore we are in the context of a function
      -- with an explicit @Member e r@ constraint. We also know it can
      -- be unified (although it may generate unsatisfiable constraints).
      Just eff' -> Just <$> mkWantedForce fc eff'

      -- Otherwise, check to see if @r ~ (e ': r')@. If so, pretend we're
      -- trying to solve a given @Member e r@. But this can only happen in the
      -- context of an interpreter!
      Nothing ->
        case splitAppTys r of
          (_, [_, eff', _]) -> do
            mkWanted fc
                     (InterpreterUse $ exactlyOneWantedForR wanted_finds r)
                     eff'
          _ -> pure Nothing

  -- We only want to emit a unification wanted once, otherwise a type error can
  -- force the type checker to loop forever.
  already_emitted <- tcPluginIO $ readIORef ref
  let unifications = unzipNewWanteds already_emitted $ catMaybes eqs
      good_unifications = getGoodUnifications unifications
      new_wanteds = fmap _unifyCt good_unifications
  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList good_unifications

  solvedFounds <- getFounds stuff wanted
  solvedIndexOfs <- getIndexOfFounds stuff wanted

  pure $ TcPluginOk (solveBogusError stuff wanted) $
    mconcat
      [ new_wanteds
      , solvedFounds
      , solvedIndexOfs
      ]


------------------------------------------------------------------------------
-- | Unroll a row into all the types that are consed together in it.
unrollR :: Type -> [Type]
unrollR r =
  case splitAppTys r of
    (_, [_, eff', r']) -> eff' : unrollR r'
    _                  -> [r]


------------------------------------------------------------------------------
-- | Solves stuck type families of the form @IndexOf row (Found row e) ~ e@.
-- This can happen when we're in interpreter mode, searching for effects in the
-- row that have type variables that should /stay/ type variables.
getIndexOfFounds :: PolysemyStuff 'Things -> [Ct] -> TcPluginM [Ct]
getIndexOfFounds stuff cts = sequenceA $ do
  ct@(CNonCanonical ev) <- cts
  let pred = ctev_pred ev
  Just (_, t1, e) <- pure $ getEqPredTys_maybe pred
  Just (indexOfTc, [_, r, n]) <- pure $ splitTyConApp_maybe t1
  guard $ indexOfTc == indexOfTyCon stuff
  Just (foundTc, [_, r', e']) <- pure $ splitTyConApp_maybe n
  guard $ foundTc == foundTyCon stuff
  guard $ eqType r r'
  guard $ eqType e e'

  let EvExpr ev = evByFiat "polysemy-plugin" t1 e
  pure $ CNonCanonical <$> newGiven (ctLoc ct) (mkPrimEqPred t1 e) ev


------------------------------------------------------------------------------
-- | Solves stuck type families of the form @Found row e ~ nat@, by determining
-- whether or not @e@ is actually at @nat@ in @row@. GHC should actually do
-- this on its own, but doesn't due to
-- https://gitlab.haskell.org/ghc/ghc/issues/17311
getFounds :: PolysemyStuff 'Things -> [Ct] -> TcPluginM [Ct]
getFounds stuff cts = sequenceA $ do
  ct@(CNonCanonical ev) <- cts
  let pred = ctev_pred ev
  Just (_, t1, nat) <- pure $ getEqPredTys_maybe pred
  Just (foundTc, [_, r, e]) <- pure $ splitTyConApp_maybe t1
  guard $ foundTc == foundTyCon stuff
  let unrolled = unrollR r
      EvExpr ev = evByFiat "polysemy-plugin" t1 nat
      proof = CNonCanonical <$> newGiven (ctLoc ct) (mkPrimEqPred t1 nat) ev

  case fmap fst $ find (eqType e . snd) $ zip [0..] unrolled of
    Just index -> do
      guard $ mkNat stuff index `eqType` nat
      pure proof
    Nothing -> do
      let n = stripS stuff nat
      case drop n unrolled of
        [t] | isTyVarTy t -> pure proof
        _ -> empty


------------------------------------------------------------------------------
-- | Turn an 'Int' into a 'Polysemy.Internal.Union.Nat'.
mkNat :: PolysemyStuff 'Things -> Int -> Type
mkNat stuff = go
  where
    go 0 = mkTyConTy $ promotedZDataCon stuff
    go n = mkTyConApp (promotedSDataCon stuff) [go $ n - 1]


------------------------------------------------------------------------------
-- | Counts the number of 'Polysemy.Internal.Union.S' constructors in a type.
stripS :: PolysemyStuff 'Things -> Type -> Int
stripS stuff = go
  where
    go (splitTyConApp_maybe -> Just (tc, [a]))
      | tc == promotedSDataCon stuff
      = 1 + go a
    go _ = 0

