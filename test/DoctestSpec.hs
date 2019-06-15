{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DoctestSpec where

import Test.DocTest
import Test.Hspec

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.Output
-- >>> :m +Polysemy.Reader
-- >>> :m +Polysemy.State
-- >>> :m +Polysemy.Trace


--------------------------------------------------------------------------------
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
ambiguousMonoState = ()


--------------------------------------------------------------------------------
-- |
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
ambiguousPolyState = ()


--------------------------------------------------------------------------------
-- |
-- TODO(sandy): should this mention 'Reader i' or just 'Reader'?
--
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
interpretBadFirstOrder = ()


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- runFoldMapOutput
--     :: forall o m r a
--      . Monoid m
--     => (o -> m)
--     -> Sem (Output o ': r) a
--     -> Sem r (m, a)
-- runFoldMapOutput f = runState mempty . reinterpret $ \case
--   Output o -> modify (<> f o)
-- :}
-- ...
-- ... 'e10' is higher-order, but 'reinterpret' can help only
-- ... with first-order effects.
-- ...
--
-- PROBLEM: Output _is_ first order! But we're not inferring `e1 ~ Output`,
-- because the real type error breaks inference. So instead we get `e10`, which
-- we can't prove is first order, so we emit the error.
--
-- SOLUTION: Don't emit the error when `e1` is a tyvar.
firstOrderReinterpret'WRONG = ()


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- let reinterpretScrub :: Sem (Output Int ': m) a -> Sem (State Bool ': Trace ': m) a
--     reinterpretScrub = undefined
--     foo :: Sem '[Output Int] ()
--     foo = pure ()
--     foo' = reinterpretScrub foo
--     foo'' = runState True foo'
--     foo''' = runTraceIO foo''
--  in runM foo'''
-- :}
-- ...
-- ... Ambiguous use of effect 'Lift'
-- ...
-- ... add (Member (Lift IO) '[]) ...
-- ...
--
-- PROBLEM: We're trying to run more effects than exist in the eff row. This is
-- indeed a problem, but the error message isn't helpful.
--
-- SOLUTION: Add a special case to `AmbiguousSend` when `r ~ '[]`.
runningTooManyEffects'WRONG = ()


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- foo :: Sem (State Int ': r) ()
-- foo = put ()
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
-- ... (Member (State ()) State Int : r) ...
-- ...
--
-- PROBLEM: There should be parentheses around `State Int : r`
--
-- SOLUTION: Emit parens only when the effect row is of the form `e1 ': ...`
missingParens'WRONG = ()



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
  , "src/Polysemy/Output.hs"
  , "src/Polysemy/Reader.hs"
  , "src/Polysemy/State.hs"
  , "src/Polysemy/Trace.hs"
  ]

