module DoctestSpec where

import Test.DocTest
import Test.Hspec

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.Reader
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
--
-- TODO(sandy): should this mention 'Reader i' or just 'Reader'?
-- >>> :{
-- interpret @Reader $ \case
--   Ask -> undefined
-- :}
-- ...
-- ... 'Reader i' is higher-order, but 'interpret' can help only
-- ... with first-order effects.
-- ...
-- ... 'interpretH' instead.
-- ...
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

  , "test/DoctestSpec.hs"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Polysemy.hs"
  , "src/Polysemy/Reader.hs"
  , "src/Polysemy/State.hs"
  ]

