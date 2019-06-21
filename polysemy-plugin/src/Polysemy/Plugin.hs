{-# LANGUAGE CPP                       #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

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
-- This is obviously insane, but it's the way the cookie crumbles.
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

import GhcPlugins

import Polysemy.Plugin.Fundep
import Polysemy.Plugin.InlineRecursiveCalls
#if __GLASGOW_HASKELL__ >= 810
import Polysemy.Plugin.Phases
#endif

------------------------------------------------------------------------------
plugin :: Plugin
plugin = defaultPlugin
    { tcPlugin            = const $ Just fundepPlugin
#if __GLASGOW_HASKELL__ >= 806
    , pluginRecompile     = purePlugin
    , renamedResultAction = \_ env group ->
        traverse inlineRecCallsAction (env, group)
    , installCoreToDos    = const installTodos
#endif
    }

------------------------------------------------------------------------------
installTodos :: [CoreToDo] -> CoreM [CoreToDo]
installTodos todos = do
  dyn_flags <- getDynFlags

  pure [ pass
       | optLevel dyn_flags > 0
       , pass <- todos
#if __GLASGOW_HASKELL__ >= 810
              ++ extraPhases dyn_flags
#endif
       ]
