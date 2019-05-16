-- | Due to a quirk of the GHC plugin interface, it's only easy to find
-- transitive dependencies if they define an orphan instance. This module
-- exists to provide some things so we can define a (safe) orphan instance in
-- the module we want to find ("Polysemy.Internal").
module Polysemy.Internal.PluginLookup where

class PluginLookup t
data Plugin

