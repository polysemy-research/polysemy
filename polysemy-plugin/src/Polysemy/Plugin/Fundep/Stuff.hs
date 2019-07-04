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
import TcPluginM (TcPluginM, tcLookupClass, tcLookupTyCon)



data PolysemyStuff (l :: LookupState) = PolysemyStuff
  { findClass    :: ThingOf l Class
  , semTyCon     :: ThingOf l TyCon
  , ifStuckTyCon :: ThingOf l TyCon
  , indexOfTyCon :: ThingOf l TyCon
  }


polysemyStuffLocations :: PolysemyStuff 'Locations
polysemyStuffLocations = PolysemyStuff
  { findClass    = ("polysemy",    "Polysemy.Internal.Union", "Find")
  , semTyCon     = ("polysemy",    "Polysemy.Internal",       "Sem")
  , ifStuckTyCon = ("type-errors", "Type.Errors",             "IfStuck")
  , indexOfTyCon = ("polysemy",    "Polysemy.Internal.Union", "IndexOf")
  }


polysemyStuff :: TcPluginM (PolysemyStuff 'Things)
polysemyStuff =
  let PolysemyStuff a b c d = polysemyStuffLocations
   in PolysemyStuff <$> doLookup a
                    <*> doLookup b
                    <*> doLookup c
                    <*> doLookup d


data LookupState
  = Locations
  | Things


type family ThingOf (l :: LookupState) (a :: Type) :: Type where
  ThingOf 'Locations _ = (String, String, String)
  ThingOf 'Things a = a


class CanLookup a where
  lookupStrategy :: Name -> TcPluginM a

instance CanLookup Class where
  lookupStrategy = tcLookupClass

instance CanLookup TyCon where
  lookupStrategy = tcLookupTyCon


doLookup :: CanLookup a => ThingOf 'Locations a -> TcPluginM (ThingOf 'Things a)
doLookup (package, mdname, name) = do
  md  <- lookupModule (mkModuleName mdname) $ fsLit package
  nm <- lookupName md $ mkTcOcc name
  lookupStrategy nm


