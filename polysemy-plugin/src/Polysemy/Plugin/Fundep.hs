{-# LANGUAGE GeneralizedNewtypeDeriving #-}

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
import           Data.Function (on)
import           Data.IORef
import           Data.List
import           Data.Maybe
import qualified Data.Set as S
import           FastString (fsLit)
import           GHC (ModuleName)
import           GHC.TcPluginM.Extra (lookupModule, lookupName)
import           Module (mkModuleName)
import           OccName (mkTcOcc)
import           TcPluginM (TcPluginM, tcLookupClass, tcPluginIO)
import           TcRnTypes
import           TcSMonad hiding (tcLookupClass)
import           TyCoRep (Type (..))
import           Type


polysemyInternalUnion :: ModuleName
polysemyInternalUnion = mkModuleName "Polysemy.Internal.Union"

fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
    { tcPluginInit = do
        md <- lookupModule polysemyInternalUnion (fsLit "polysemy")
        monadEffectTcNm <- lookupName md (mkTcOcc "Find")
        (,) <$> tcPluginIO (newIORef S.empty)
            <*> tcLookupClass monadEffectTcNm
    , tcPluginSolve = solveFundep
    , tcPluginStop = const (return ()) }

allMonadEffectConstraints :: Class -> [Ct] -> [(CtLoc, (Type, Type, Type))]
allMonadEffectConstraints cls cts =
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


canUnifyRecursive :: Bool -> Type -> Type -> Bool
canUnifyRecursive poly_given_ok = go True
  where
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
    :: Bool
    -> Bool
    -> CtLoc
    -> Type
    -> Type
    -> TcPluginM (Maybe ( (OrdType, OrdType)  -- the types we want to unify
                        , Ct                  -- the constraint
                        ))
mkWanted must_unify poly_given_ok loc wanted given =
  whenA (not must_unify || canUnifyRecursive poly_given_ok wanted given) $ do
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



solveFundep
    :: (IORef (S.Set (OrdType, OrdType)), Class)
    -> [Ct]
    -> [Ct]
    -> [Ct]
    -> TcPluginM TcPluginResult
solveFundep _ _ _ [] = pure $ TcPluginOk [] []
solveFundep (ref, effCls) giv _ want = do
    let wantedEffs = allMonadEffectConstraints effCls want
        givenEffs = snd <$> allMonadEffectConstraints effCls giv
        num_wanteds_by_r = countLength eqType $ fmap (thd . snd) wantedEffs
        must_unify r =
          let Just num_wanted = find (eqType r . fst) num_wanteds_by_r
           in snd num_wanted /= 1

    eqs <- forM wantedEffs $ \(loc, e@(_, eff, r)) -> do
      case findMatchingEffectIfSingular e givenEffs of
        Nothing -> do
          case splitAppTys r of
            (_, [_, eff', _]) -> mkWanted (must_unify r) True loc eff eff'
            _                 -> pure Nothing
        Just eff' -> mkWanted True False loc eff eff'

    already_emitted <- tcPluginIO $ readIORef ref
    let new_wanteds = filter (not . flip S.member already_emitted . fst)
                    $ catMaybes eqs

    tcPluginIO $ modifyIORef ref $ S.union $ S.fromList $ fmap fst new_wanteds
    pure . TcPluginOk [] $ fmap snd new_wanteds

