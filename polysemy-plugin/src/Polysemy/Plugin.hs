{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE CPP #-}

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


------------------------------------------------------------------------------
-- | A typechecker plugin that can disambiguate "obvious" uses of effects in
-- Polysemy.
--
-- __Example:__
--
-- Consider the following program:
--
-- @
-- foo :: 'Polysemy.Member' ('Polysemy.State.State' Int) r => 'Polysemy.Sem' r ()
-- foo = 'Polysemy.State.put' 10
-- @
--
-- What does this program do? Any human will tell you that it changes the state
-- of the 'Int' to 10, which is clearly what's meant.
--
-- Unfortunately, Polysemy can't work this out on its own. Its reasoning is
-- "maybe you wanted to change some other 'Polysemy.State.State' effect which
-- is /also/ a 'Num', but you just forgot to add a 'Polysemy.Member' constraint
-- for it."
--
-- This is obbviously insane, but it's the way the cookie crumbles.
-- 'Polysemy.Plugin' is a typechecker plugin which will disambiguate the above
-- program (and others) so the compiler will do what you want.
--
-- __Usage:__
--
-- Add the following line to your package configuration:
--
-- @
-- ghc-options: -fplugin=Polysemy.Plugin
-- @
--
-- __Limitations:__
--
-- The 'Polysemy.Plugin' will only disambiguate effects if there is exactly one
-- relevant constraint in scope. For example, it will /not/ disambiguate the
-- following program:
--
-- @
-- bar :: 'Polysemy.Members' \'[ 'Polysemy.State.State' Int
--                 , 'Polysemy.State.State' Double
--                 ] r => 'Polysemy.Sem' r ()
-- bar = 'Polysemy.State.put' 10
-- @
--
-- because it is now unclear whether you're attempting to set the 'Int' or the
-- 'Double'. Instead, you can manually write a type application in this case.
--
-- @
-- bar :: 'Polysemy.Members' \'[ 'Polysemy.State.State' Int
--                 , 'Polysemy.State.State' Double
--                 ] r => 'Polysemy.Sem' r ()
-- bar = 'Polysemy.State.put' @Int 10
-- @
--
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
        md <- lookupModule (mkModuleName "Polysemy.Internal.Union") (fsLit "polysemy")
        monadEffectTcNm <- lookupName md (mkTcOcc "Find")
        tcLookupClass monadEffectTcNm
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


mkWanted :: CtLoc -> Type -> Type -> TcPluginM (Maybe Ct)
mkWanted loc eff eff' = do
  if eqType (getEffName eff) (getEffName eff')
     then do
       (ev, _) <- unsafeTcPluginTcM $ runTcSDeriveds $ newWantedEq loc Nominal eff eff'
       pure $ Just (CNonCanonical ev)
     else
       pure Nothing


solveFundep :: Class -> [Ct] -> [Ct] -> [Ct] -> TcPluginM TcPluginResult
solveFundep effCls giv _ want = do
    let wantedEffs = allMonadEffectConstraints effCls want
    let givenEffs = snd <$> allMonadEffectConstraints effCls giv
    eqs <- forM wantedEffs $ \(loc, e@(_, eff, r)) ->
      case findMatchingEffectIfSingular e givenEffs of
        Nothing -> do
          case splitAppTys r of
            (_, [_, eff', _]) -> mkWanted loc eff eff'
            _                 -> pure Nothing
        Just eff' -> mkWanted loc eff eff'

    return (TcPluginOk [] (catMaybes eqs))

