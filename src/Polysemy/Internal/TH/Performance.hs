{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.TH.Performance
  ( inlineRecursiveCalls
  ) where

import Language.Haskell.TH

{-# DEPRECATED inlineRecursiveCalls
      "Enabling polysemy-plugin now automatically inlines recursive calls"
      #-}

inlineRecursiveCalls :: Q [Dec] -> Q [Dec]
inlineRecursiveCalls = id

