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
-- runOutputMonoid
--     :: forall o m r a
--      . Monoid m
--     => (o -> m)
--     -> Sem (Output o ': r) a
--     -> Sem r (m, a)
-- runOutputMonoid f = runState mempty . reinterpret $ \case
--   Output o -> modify (`mappend` f o)
-- :}
-- ...
-- ... Probable cause: ...reinterpret... is applied to too few arguments
-- ...
tooFewArgumentsReinterpret = ()


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- let foo :: Member Resource r => Sem r ()
--     foo = undefined
--  in runM $ lowerResource foo
-- :}
-- ...
-- ... Couldn't match expected type...
-- ... with actual type...
-- ... Probable cause: ... is applied to too few arguments
-- ...
missingArgumentToRunResourceInIO = ()

