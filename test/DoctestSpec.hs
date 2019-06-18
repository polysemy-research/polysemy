{-# LANGUAGE CPP #-}

module DoctestSpec where

import Test.DocTest
import Test.Hspec

spec :: Spec
spec = parallel $ describe "Error messages" $ it "should pass the doctest" $ doctest
  [ "-isrc/"
  , "--fast"
  , "-XDataKinds"
  , "-XDeriveFunctor"
  , "-XFlexibleContexts"
  , "-XGADTs"
  , "-XLambdaCase"
  , "-XPolyKinds"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeApplications"
  , "-XTypeFamilies"
  , "-XTypeOperators"
  , "-XUnicodeSyntax"

#if __GLASGOW_HASKELL__ < 806
  , "-XMonadFailDesugaring"
  , "-XTypeInType"
#endif

  , "test/TypeErrors.hs"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Polysemy.hs"
  , "src/Polysemy/Output.hs"
  , "src/Polysemy/Reader.hs"
  , "src/Polysemy/Resource.hs"
  , "src/Polysemy/State.hs"
  , "src/Polysemy/Trace.hs"
  ]

