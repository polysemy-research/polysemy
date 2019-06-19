{-# LANGUAGE CPP #-}

module Polysemy.Plugin.Phases
  ( extraPhases
  ) where

import BasicTypes
import CoreMonad
import DynFlags

extraPhases :: DynFlags -> [CoreToDo]
extraPhases dflags =
    [ CoreDoSpecialising
    , simpl_phase 0 ["post-late-spec"] max_iter
    , simpl_gently
    , CoreDoStaticArgs
    , CoreDoSpecialising
    -- TODO(sandy): probably don't need this one
    , simpl_phase 0 ["post-late-spec"] max_iter
    , simpl_phases
    , simpl_gently
    ]

  where
    option   = flip gopt dflags
    max_iter = maxSimplIterations dflags
    rules_on = option Opt_DoLambdaEtaExpansion
    phases   = simplPhases dflags

    base_mode = SimplMode
      { sm_phase      = error "base_mode"
      , sm_names      = []
#if __GLASGOW_HASKELL__ >= 804
      , sm_dflags     = dflags
#endif
      , sm_rules      = option Opt_EnableRewriteRules
      , sm_eta_expand = rules_on
      , sm_inline     = True
      , sm_case_case  = True
      }

    simpl_phase phase names iter = CoreDoPasses
      [ runWhen (phase `elem` strictnessBefore dflags) CoreDoStrictness
      , CoreDoSimplify iter $
          base_mode { sm_phase = Phase phase
                    , sm_names = names
                    }
      , runMaybe (ruleCheck dflags) $ CoreDoRuleCheck $ Phase phase
      ]

    simpl_gently = CoreDoSimplify max_iter $ base_mode
      { sm_phase = InitialPhase
      , sm_names = ["Gentle"]
      , sm_rules = rules_on
      , sm_inline = True
      , sm_case_case = False
      }

    simpl_phases = CoreDoPasses
      [ simpl_phase phase ["main"] max_iter
      | phase <- [phases, phases-1 .. 1]
      ]

