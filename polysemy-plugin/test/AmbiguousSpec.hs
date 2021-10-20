{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module AmbiguousSpec where

import Polysemy
import Test.Hspec
import Polysemy.State
import Data.Monoid

-- ambiguous :: Members '[
--               State Int
--             , State String
--             ] r
--           => Sem r ()
-- ambiguous = put 10

ambiguous1 :: Members '[State (Sum Int), State String] r => Sem r ()
ambiguous1 = put mempty

-- ambiguous2 :: (Num String, Members '[State Int, State String] r) => Sem r ()
-- ambiguous2 = put 10

-- spec :: Spec
-- spec = describe "example" $ do
--   it "should compile!" $ do
--     let z = run . runState 0 . runState "hello" $ ambiguous
--     z `shouldBe` (10, ("hello", ()))

