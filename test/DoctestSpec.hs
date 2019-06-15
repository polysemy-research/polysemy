module DoctestSpec where

import Test.DocTest
import Test.Hspec

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.State

-- |
-- >>> :{
-- foo :: Sem r ()
-- foo = put ()
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
-- ... (Member (State ()) r) ...
-- ...
--
--
-- >>> :{
-- foo :: Sem r ()
-- foo = put 5
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
-- ... (Member (State s0) r) ...
-- ...
-- ... 's0' directly...
-- ...
spec :: Spec
spec = describe "Error messages" $ it "should pass the doctest" $ doctest
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

  , "test/DoctestSpec.hs"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Polysemy.hs"
  , "src/Polysemy/State.hs"
  ]

