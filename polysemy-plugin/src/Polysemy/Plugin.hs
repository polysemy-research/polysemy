{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

module Polysemy.Plugin
  ( plugin
  ) where

-- external
import GHC.TcPluginM.Extra (lookupModule, lookupName)

-- GHC API
import FastString (fsLit)
import Module     (mkModuleName)
import OccName    (mkTcOcc)
import Plugins    (Plugin (..), defaultPlugin
#if __GLASGOW_HASKELL__ >= 806
    , PluginRecompile(..)
#endif
    )
import TcPluginM  (TcPluginM, tcLookupClass)
import TcRnTypes
import TyCoRep    (Type (..))
import Control.Monad
import Class
import Type
import Data.Maybe
import TcSMonad hiding (tcLookupClass)
import CoAxiom
import Outputable

plugin :: Plugin
plugin = defaultPlugin
    { tcPlugin = const (Just fundepPlugin)
#if __GLASGOW_HASKELL__ >= 806
    , pluginRecompile = const (return NoForceRecompile)
#endif
    }

fundepPlugin :: TcPlugin
fundepPlugin = TcPlugin
    { tcPluginInit = do
        md <- lookupModule (mkModuleName "Polysemy") (fsLit "polysemy")
        monadEffectTcNm <- lookupName md (mkTcOcc "Member")
        tcLookupClass monadEffectTcNm
    , tcPluginSolve = solveFundep
    , tcPluginStop = const (return ()) }

allMonadEffectConstraints :: Class -> [Ct] -> [(CtLoc, (Type, Type, Type))]
allMonadEffectConstraints cls cts =
    [ (ctLoc cd, (effName, eff, mon))
        | cd@CDictCan{cc_class = cls', cc_tyargs = [eff, mon]} <- cts
        , cls == cls'
        , let (effName, _) = splitAppTys eff ]

singleListToJust :: [a] -> Maybe a
singleListToJust [a] = Just a
singleListToJust _ = Nothing

findMatchingEffectIfSingular :: (Type, Type, Type) -> [(Type, Type, Type)] -> Maybe Type
findMatchingEffectIfSingular (effName, _, mon) ts = singleListToJust
    [ eff'
        | (effName', eff', mon') <- ts
        , eqType effName effName'
        , eqType mon mon' ]

solveFundep :: Class -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFundep effCls giv _ want = do
    pprPanic "wat" $ ppr $ giv ++ want
    let wantedEffs = allMonadEffectConstraints effCls want
    let givenEffs = snd <$> allMonadEffectConstraints effCls giv
    eqs <- forM wantedEffs $ \(loc, e@(_, eff, _)) ->
        case findMatchingEffectIfSingular e givenEffs of
            Nothing -> return Nothing
            Just eff' -> do
                (ev, _) <- unsafeTcPluginTcM
                    (runTcSDeriveds (newWantedEq loc Nominal eff eff'))
                return (Just (CNonCanonical ev))
    return (TcPluginOk [] (catMaybes eqs))

