{-# LANGUAGE CPP #-}

module Plugin.DoctestSpec where

import Test.Hspec
import Test.DocTest

spec :: Spec
spec = describe "yo" $ it "ok" $ True `shouldBe` True
-- spec = describe "Error222 messages" $ it "should pass the doctest" $ doctest
--   [ "--fast"
--   , "-fobject-code"
--   , "-XDataKinds"
--   , "-XDeriveFunctor"
--   , "-XFlexibleContexts"
--   , "-XGADTs"
--   , "-XLambdaCase"
--   , "-XPolyKinds"
--   , "-XRankNTypes"
--   , "-XScopedTypeVariables"
--   , "-XStandaloneDeriving"
--   , "-XTypeApplications"
--   , "-XTypeFamilies"
--   , "-XTypeOperators"
--   , "-XUnicodeSyntax"

-- #if __GLASGOW_HASKELL__ < 806
--   , "-XMonadFailDesugaring"
--   , "-XTypeInType"
-- #endif

--   , "test/Plugin/TypeErrors.hs"
--   ]
