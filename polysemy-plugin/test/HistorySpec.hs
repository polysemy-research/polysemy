{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeOperators       #-}

{-# OPTIONS_GHC
     -fplugin=Polysemy.Plugin
     -fdefer-type-errors
     #-}

module HistorySpec where

import Polysemy
import Polysemy.State
import Polysemy.Internal
import Polysemy.Internal.Union
import Data.Functor.Identity
import Test.Hspec


spec :: Spec
spec = describe "HistorySpec" $ it "should compile" $ True `shouldBe` True


runHistory
    :: Sem (State (Identity s) ': State s ': r) ()
runHistory = do
  s <- gets runIdentity
  put s

