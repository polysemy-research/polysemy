{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TypeErrors where

-- $setup
-- >>> default ()
-- >>> :set -package polysemy-plugin
-- ...
-- >>> :set -fplugin=Polysemy.Plugin
-- >>> :m +Polysemy
-- >>> :m +Polysemy.State
-- >>> :m +Data.Maybe


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- existsKV :: Member (State (Maybe Int)) r => Sem r Bool
-- existsKV = isJust get
-- :}
-- ...
-- ... Couldn't match expected type ...Sem r Bool... with actual type ...Bool...
-- ...
-- ... Couldn't match expected type ...Maybe a0...
-- ... with actual type ...Sem r0 s0...
-- ...
missingFmap = ()

