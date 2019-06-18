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
  , "-XTypeOperators"
  , "-XTypeFamilies"
  , "-XUnicodeSyntax"

  , "test/TypeErrors.hs"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Polysemy.hs"
  , "src/Polysemy/Output.hs"
  , "src/Polysemy/Reader.hs"
  , "src/Polysemy/Resource.hs"
  , "src/Polysemy/State.hs"
  , "src/Polysemy/Trace.hs"
  ]

