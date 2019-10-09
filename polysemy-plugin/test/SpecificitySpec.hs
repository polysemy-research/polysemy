{-# LANGUAGE AllowAmbiguousTypes #-}
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

{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module SpecificitySpec where

import Polysemy
import Polysemy.State
import Polysemy.Internal
import Polysemy.Internal.Union
import Test.Hspec
import Data.Functor.Identity


spec :: Spec
spec = describe "HistorySpec" $ it "should compile" $ True `shouldBe` True

foo :: Members [State s, State (Identity s)] r => Sem r ()
foo = do
  s <- get
  put $ Identity s
