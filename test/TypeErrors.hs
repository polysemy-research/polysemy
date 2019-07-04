{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module TypeErrors where

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.Output
-- >>> :m +Polysemy.Reader
-- >>> :m +Polysemy.Resource
-- >>> :m +Polysemy.State
-- >>> :m +Polysemy.Trace
-- >>> :m +Data.Maybe


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
-- >>> :{
-- interpret @(Reader Bool) $ \case
--   Ask -> undefined
-- :}
-- ...
-- ... 'Reader Bool' is higher-order, but 'interpret' can help only
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
--   Output o -> modify (`mappend` f o)
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
-- ... Unhandled effect 'Lift IO'
-- ...
-- ... Expected type: Sem '[Lift m] (Bool, ())
-- ... Actual type: Sem '[] (Bool, ())
-- ...
runningTooManyEffects = ()


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
-- ... Couldn't match expected type ...
-- ... with actual type ...
-- ... Probable cause: ... is applied to too few arguments
-- ...
missingArgumentToRunResourceInIO = ()


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- existsKV :: Member (State (Maybe Int)) r => Sem r Bool
-- existsKV = isJust get
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
--
-- PROBLEM: because this doesn't typecheck, `get` has type `Sem r0 a1`, which
-- truly is ambiguous!
--
-- SOLUTION: maybe the plugin can detect the case that we're trying to compare
-- a `Sem r` with something completely unrelated, and then just emit bogus
-- `Find` dictionaries?
missingFmap'WRONG = ()

