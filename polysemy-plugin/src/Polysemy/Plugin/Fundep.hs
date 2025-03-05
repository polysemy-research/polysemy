{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
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

import           Control.Monad
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.State
import           Data.Bifunctor
import           Data.Coerce
import           Data.Function (on)
import           Data.IORef
import qualified Data.Map as M
import           Data.Maybe
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.Traversable (for)
import           Polysemy.Plugin.Fundep.Stuff
import           Polysemy.Plugin.Fundep.Unification
import           Polysemy.Plugin.Fundep.Utils

#if __GLASGOW_HASKELL__ >= 906
#define SUBST Subst
#define GET_TYVAR getTyVar_maybe
import           GHC.Core.TyCo.Subst (SUBST)
import           GHC.Core.TyCo.Compare (eqType, nonDetCmpType)
#else
#define SUBST TCvSubst
#define GET_TYVAR tcGetTyVar_maybe
#endif

#if __GLASGOW_HASKELL__ >= 912
import           GHC.Tc.Types.CtLoc
#endif

#if __GLASGOW_HASKELL__ >= 900
import           GHC.Builtin.Types.Prim (alphaTys)
import           GHC.Plugins (idType, tyConClass_maybe, ppr, Outputable, sep, text, (<+>), parens, emptyUFM)
import           GHC.Tc.Types.Evidence
import           GHC.Tc.Plugin (TcPluginM, tcPluginIO)
import           GHC.Tc.Types
import           GHC.Tc.Types.Constraint
import           GHC.Tc.Utils.Env (tcGetInstEnvs)
import           GHC.Tc.Utils.TcType (tcSplitPhiTy, tcSplitTyConApp, GET_TYVAR, tcSplitAppTy_maybe)
import           GHC.Tc.Solver.Monad hiding (tcLookupClass)
import           GHC.Core.Class (classTyCon)
import           GHC.Core.InstEnv (lookupInstEnv, is_dfun)
import           GHC.Core.Type
import           GHC.Utils.Monad (allM, anyM)

#else
#if __GLASGOW_HASKELL__ >= 810
import           Constraint
#endif

import           Class (classTyCon)
import           GhcPlugins (idType, tyConClass_maybe, ppr, Outputable, sep, text, (<+>), parens)
import           Inst (tcGetInstEnvs)
import           InstEnv (lookupInstEnv, is_dfun)
import           MonadUtils (allM, anyM)
import           TcEvidence
import           TcPluginM (tcPluginIO)
import           TcRnTypes
import           TcType (tcSplitPhiTy, tcSplitTyConApp, GET_TYVAR, tcSplitAppTy_maybe)
import           TcSMonad hiding (tcLookupClass)
import           Type
import           TysPrim (alphaTys)
#endif

fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
  { tcPluginInit =
      (,) <$> tcPluginIO (newIORef S.empty)
          <*> polysemyStuff
  , tcPluginSolve = solveFundep
  , tcPluginStop = const $ pure ()
#if __GLASGOW_HASKELL__ >= 904
  , tcPluginRewrite = \ _ -> emptyUFM
#endif
  }


------------------------------------------------------------------------------
-- | Like 'PredType', but has an 'Ord' instance.
newtype PredType' = PredType' { getPredType :: PredType }
  deriving newtype Outputable

instance Eq PredType' where
  (==) = ((== EQ) .) . compare

instance Ord PredType' where
  compare = nonDetCmpType `on` getPredType


------------------------------------------------------------------------------
-- | Corresponds to a 'Polysemy.Internal.Union.Find' constraint. For example,
-- given @Member (State s) r@, we would get:
data FindConstraint = FindConstraint
  { fcLoc        :: CtLoc
  , fcEffectName :: Type  -- ^ @State@
  , fcEffect     :: Type  -- ^ @State s@
  , fcRow        :: Type  -- ^ @r@
  }

instance Outputable FindConstraint where
  ppr FindConstraint{..} = parens $ sep
    [ text "effect name = " <+> ppr fcEffectName
    , text "effect = " <+> ppr fcEffect
    , text "row = " <+> ppr fcRow
    ]


------------------------------------------------------------------------------
-- | Given a list of constraints, filter out the 'FindConstraint's.
getFindConstraints :: PolysemyStuff 'Things -> [Ct] -> [FindConstraint]
getFindConstraints (findClass -> cls) cts = do
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
  cd@(CDictCan(DictCt{di_cls = cls', di_tys = [eff, r]})) <- cts
#else
  cd@CDictCan{cc_class = cls', cc_tyargs = [eff, r]} <- cts
#endif
  guard $ cls == cls'
  pure $ FindConstraint
    { fcLoc = ctLoc cd
    , fcEffectName = getEffName eff
    , fcEffect = eff
    , fcRow = r
    }


------------------------------------------------------------------------------
-- | Get evidence in scope that aren't the 'FindConstraint's.
getExtraEvidence :: PolysemyStuff 'Things -> [Ct] -> [PredType]
getExtraEvidence things cts = do
#if MIN_VERSION_GLASGOW_HASKELL(9,8,0,0)
  CDictCan(DictCt{di_cls = cls, di_tys = as}) <- cts
#else
  CDictCan{cc_class = cls, cc_tyargs = as} <- cts
#endif
  guard $ cls /= findClass things
  pure $ mkAppTys (mkTyConTy $ classTyCon cls) as


------------------------------------------------------------------------------
-- | If there's a unique given @Member@ that would cause the program to
-- typecheck, use it.
findMatchingEffectIfSingular
    :: [PredType]        -- ^ Extra wanteds
    -> Set PredType'     -- ^ Extra givens
    -> FindConstraint    -- ^ Goal
    -> [FindConstraint]  -- ^ Member constraints
    -> TcM (Maybe Type)
findMatchingEffectIfSingular
    extra_wanted
    extra_given
    (FindConstraint _ eff_name wanted r)
    ts =
  let skolems = S.fromList $ foldMap (tyCoVarsOfTypeWellScoped . fcEffect) ts
      -- Which members unify with our current goal?
      results = do
        FindConstraint _ eff_name' eff' r' <- ts
        guard $ eqType eff_name eff_name'
        guard $ eqType r r'
        subst <- maybeToList $ unify (FunctionDef skolems) wanted eff'
        pure (eff', subst)
   in case results of
        [] -> pure Nothing
        -- If there is a unique member which unifies, return it.
        [(a, _)] -> pure $ Just a
        _ ->
          -- Otherwise, check if the extra wanteds give us enough information
          -- to make a unique choice.
          --
          -- For example, if we're trying to solve @Member (State a) r@, with
          -- candidates @Members (State Int, State String) r@ and can prove
          -- that @Num a@, then we can uniquely choose @State Int@.
          fmap (singleListToJust . join) $ for results $ \(eff, subst) ->
            fmap maybeToList $
              anyM (checkExtraEvidence extra_given subst) extra_wanted >>= \case
                True -> pure $ Just eff
                False -> pure Nothing

------------------------------------------------------------------------------
-- | @checkExtraEvidence givens subst c@ returns 'True' iff we can prove that
-- the constraint @c@ holds under the substitution @subst@ in the context of
-- @givens@.
checkExtraEvidence ::
  Set PredType' ->
  SUBST ->
  PredType ->
  TcM Bool
checkExtraEvidence givens subst = flip evalStateT givens . getInstance . substTy subst


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
  -> TcPluginM (Unification, Ct)
mkWantedForce fc given = do
#if __GLASGOW_HASKELL__ >= 904
  ((ev, _), _) <- unsafeTcPluginTcM
           . runTcS
           $ newWantedEq (fcLoc fc) emptyRewriterSet Nominal wanted given
#else
  (ev, _) <- unsafeTcPluginTcM
           . runTcSDeriveds
           $ newWantedEq (fcLoc fc) Nominal wanted given
#endif
  pure ( Unification (OrdType wanted) (OrdType given)
       , CNonCanonical ev
       )
  where
    wanted = fcEffect fc


------------------------------------------------------------------------------
-- | It's very important that we don't try to unify entire effects when we're
-- in interpreter mode. It's OK to unify @T x ~ T y@, but never @e ~ T y@. This
-- function takes then "given" of an interpreter, and produces a singleton
-- skolem set iff the outermost effect to be unified is a tyvar.
skolemsForInterpreter :: Type -> Set TyVar
skolemsForInterpreter ty =
  case tcSplitAppTy_maybe ty of
    Just (GET_TYVAR -> Just skolem, _) -> S.singleton skolem
    _ -> maybe mempty S.singleton $ GET_TYVAR ty


------------------------------------------------------------------------------
-- | Generate a wanted unification for the effect described by the
-- 'FindConstraint' and the given effect --- if they can be unified in this
-- context.
mkWanted
    :: FindConstraint
    -> SolveContext
    -> Type  -- ^ The given effect.
    -> TcPluginM (Maybe (Unification, Ct))
mkWanted fc solve_ctx given = do
  whenA (not (mustUnify solve_ctx) || isJust (unify solve_ctx wanted given)) $
    mkWantedForce fc given
  where
    wanted = fcEffect fc


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
               $ OrdType . fcRow <$> wanteds


------------------------------------------------------------------------------
-- | Returns 'True' if we can prove the given 'PredType' has a (fully
-- instantiated) instance. Uses 'StateT' to cache the results of any instances
-- it needs to prove in service of the original goal.
getInstance :: PredType -> StateT (Set PredType') TcM Bool
getInstance predty = do
  givens <- get
  case S.member (PredType' predty) givens of
    True -> pure True
    False ->
      let (con, apps) = tcSplitTyConApp predty
      in case tyConClass_maybe con of
        Nothing -> pure False
        Just cls -> do
          env <- lift tcGetInstEnvs
          let (mres, _, _) = lookupInstEnv False env cls apps
          case mres of
            ((inst, mapps) : _) -> do
              -- Get the instantiated type of the dictionary
              let df = piResultTys (idType $ is_dfun inst)
                    $ zipWith fromMaybe alphaTys mapps
              -- pull off its resulting arguments
              let (theta, _) = tcSplitPhiTy df
              allM getInstance theta >>= \case
                True -> do
                  -- Record that we can solve this instance, in case it's used
                  -- elsewhere
                  modify $ S.insert $ coerce predty
                  pure True
                False -> pure False
            _ -> pure False


solveFundep
    :: ( IORef (S.Set Unification)
       , PolysemyStuff 'Things
       )
#if __GLASGOW_HASKELL__ >= 904
    -> EvBindsVar
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginSolveResult
#else
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
#endif
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
#if __GLASGOW_HASKELL__ >= 904
solveFundep (ref, stuff) _ given wanted = do
#else
solveFundep (ref, stuff) given _ wanted = do
#endif
  let wanted_finds = getFindConstraints stuff wanted
      given_finds  = getFindConstraints stuff given
      extra_wanted = getExtraEvidence stuff wanted
      extra_given = S.fromList $ coerce $ getExtraEvidence stuff given

  eqs <- forM wanted_finds $ \fc -> do
    let r  = fcRow fc
    res <- unsafeTcPluginTcM
         $ findMatchingEffectIfSingular extra_wanted extra_given fc given_finds
    case res of
      -- We found a real given, therefore we are in the context of a function
      -- with an explicit @Member e r@ constraint. We also know it can
      -- be unified (although it may generate unsatisfiable constraints).
      Just eff' -> Just <$> mkWantedForce fc eff'

      -- Otherwise, check to see if @r ~ (e ': r')@. If so, pretend we're
      -- trying to solve a given @Member e r@. But this can only happen in the
      -- context of an interpreter!
      Nothing ->
        case splitAppTys r of
          (_, [_, eff', _]) ->
            mkWanted fc
              (InterpreterUse
                (exactlyOneWantedForR wanted_finds r)
                (skolemsForInterpreter eff'))
              eff'
          _ -> pure Nothing

  -- We only want to emit a unification wanted once, otherwise a type error can
  -- force the type checker to loop forever.
  already_emitted <- tcPluginIO $ readIORef ref
  let (unifications, new_wanteds) = unzipNewWanteds already_emitted $ catMaybes eqs
  tcPluginIO $ modifyIORef ref $ S.union $ S.fromList unifications

  pure $ TcPluginOk [] new_wanteds

