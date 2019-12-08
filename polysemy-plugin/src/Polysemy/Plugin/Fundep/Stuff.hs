{-# LANGUAGE CPP #-}

module Polysemy.Plugin.Fundep.Stuff
  ( PolysemyStuff (..)
  , LookupState (..)
  , polysemyStuff
  ) where

import Data.Kind (Type)
import FastString (fsLit)
import GHC (Name, Class, TyCon, mkModuleName)
import GHC.TcPluginM.Extra (lookupModule, lookupName)
import OccName (mkTcOcc)
import TcPluginM (TcPluginM, tcLookupClass, tcLookupTyCon, unsafeTcPluginTcM)
import GhcPlugins (getDynFlags)
import Packages (lookupModuleWithSuggestions, LookupResult (..))
import Outputable (pprPanic, empty, text, (<+>), ($$))



------------------------------------------------------------------------------
-- | All of the things from "polysemy" that we need access to in the plugin.
-- When @l ~ 'Locations@, each of these is just a pair of strings. When @l
-- ~ 'Things@, it's actually references to the stuff.
data PolysemyStuff (l :: LookupState) = PolysemyStuff
  { findClass         :: ThingOf l Class
  , semTyCon          :: ThingOf l TyCon
  , ifStuckTyCon      :: ThingOf l TyCon
  , locateEffectTyCon :: ThingOf l TyCon
  }


------------------------------------------------------------------------------
-- | All of the things we need to lookup.
polysemyStuffLocations :: PolysemyStuff 'Locations
polysemyStuffLocations = PolysemyStuff
  { findClass         = ("Polysemy.Internal.Union",                  "Find")
  , semTyCon          = ("Polysemy.Internal",                        "Sem")
  , ifStuckTyCon      = ("Polysemy.Internal.CustomErrors.Redefined", "IfStuck")
  , locateEffectTyCon = ("Polysemy.Internal.Union",                  "LocateEffect")
  }


------------------------------------------------------------------------------
-- | Lookup all of the 'PolysemyStuff'.
polysemyStuff :: TcPluginM (PolysemyStuff 'Things)
polysemyStuff = do
  dflags <- unsafeTcPluginTcM getDynFlags

  let error_msg = pprPanic "polysemy-plugin"
          $ text ""
         $$ text "--------------------------------------------------------------------------------"
         $$ text "`polysemy-plugin` is loaded, but"
        <+> text "`polysemy` isn't available as a package."
         $$ text "Probable fix: add `polysemy` to your cabal `build-depends`"
         $$ text "--------------------------------------------------------------------------------"
         $$ text ""
  case lookupModuleWithSuggestions dflags (mkModuleName "Polysemy") Nothing of
    LookupHidden _ _ -> error_msg
    LookupNotFound _ -> error_msg
#if __GLASGOW_HASKELL__ >= 806
    LookupUnusable _ -> error_msg
#endif
    _                -> pure ()

  let PolysemyStuff a b c d = polysemyStuffLocations
  PolysemyStuff <$> doLookup a
                <*> doLookup b
                <*> doLookup c
                <*> doLookup d


------------------------------------------------------------------------------
-- | Data kind for 'ThingOf'.
data LookupState
  = Locations
  | Things


------------------------------------------------------------------------------
-- | HKD indexed by the 'LookupState'; used by 'PolysemyStuff'.
type family ThingOf (l :: LookupState) (a :: Type) :: Type where
  ThingOf 'Locations _ = (String, String)
  ThingOf 'Things    a = a


------------------------------------------------------------------------------
-- | Things that can be found in a 'TcPluginM' environment.
class CanLookup a where
  lookupStrategy :: Name -> TcPluginM a

instance CanLookup Class where
  lookupStrategy = tcLookupClass

instance CanLookup TyCon where
  lookupStrategy = tcLookupTyCon


------------------------------------------------------------------------------
-- | Transform a @'ThingOf' 'Locations@ into a @'ThingOf' 'Things@.
doLookup :: CanLookup a => ThingOf 'Locations a -> TcPluginM (ThingOf 'Things a)
doLookup (mdname, name) = do
  md <- lookupModule (mkModuleName mdname) $ fsLit "polysemy"
  nm <- lookupName md $ mkTcOcc name
  lookupStrategy nm

