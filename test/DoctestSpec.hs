{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DoctestSpec where

import Test.DocTest
import Test.Hspec

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.Output
-- >>> :m +Polysemy.Reader
-- >>> :m +Polysemy.Resource
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
-- ... Probable cause: ...reinterpret... is applied to too few arguments
-- ...
tooFewArgumentsReinterpret = ()


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


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- let foo :: Member Resource r => Sem r ()
--     foo = undefined
--  in runM $ runResourceInIO foo
-- :}
-- ...
-- ... Ambiguous use of effect 'Lift'
-- ...
-- ... (Member (Lift IO) r0) ...
-- ...
-- ... Could not deduce: (Member Resource r1)
-- ...
--
-- PROBLEM: This error is totally bogus. We forgot to give an argument to
-- 'runResourceInIO'. For comparison, the standard error GHC gives in this case
-- is significantly more helpful:
--
--    <interactive>:192:13: error:
--        • Couldn't match expected type ‘Sem '[Lift m] a’
--                      with actual type ‘Sem (Resource : r0) a0 -> Sem r0 a0’
--        • Probable cause: ‘runResourceInIO’ is applied to too few arguments
--          In the second argument of ‘($)’, namely ‘runResourceInIO foo’
--          In the expression: runM $ runResourceInIO foo
--          In the expression:
--            let
--              foo :: Member Resource r => Sem r ()
--              foo = undefined
--            in runM $ runResourceInIO foo
--        • Relevant bindings include
--            it :: m a (bound at <interactive>:190:2)
--    <interactive>:192:29: error:
--        • Couldn't match expected type ‘Sem r0 x -> IO x’
--                      with actual type ‘Sem r1 ()’
--        • In the first argument of ‘runResourceInIO’, namely ‘foo’
--          In the second argument of ‘($)’, namely ‘runResourceInIO foo’
--          In the expression: runM $ runResourceInIO foo
--
--
-- SOLUTION: Honestly I'm not sure!
missingArgumentToRunResourceInIO'WRONG = ()



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
  , "src/Polysemy/Resource.hs"
  , "src/Polysemy/State.hs"
  , "src/Polysemy/Trace.hs"
  ]

