{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}

module TypeErrors where

-- $setup
-- >>> default ()
-- >>> :set -package polysemy-plugin
-- ...
-- >>> :set -fplugin=Polysemy.Plugin
-- >>> :m +Polysemy
-- >>> :m +Polysemy.State
-- >>> :m +Polysemy.Reader
-- >>> :m +Data.Maybe


--------------------------------------------------------------------------------
-- |
-- >>> :{
-- existsKV :: Member (State (Maybe Int)) r => Sem r Bool
-- existsKV = isJust get
-- :}
-- ...
-- ... Couldn't match expected type ...Sem r Bool... with actual type ...Bool...
-- ...
-- ... Couldn't match expected type...Maybe a0...
-- ... with actual type...Sem r0 s0...
-- ...
missingFmap = ()

--------------------------------------------------------------------------------
-- |
-- >>> :{
-- insertSome :: ∀ e1 e2 s r a . Sem (e1 ': e2 ': r) a -> Sem (e1 ': e2 ': State s ': r) a
-- insertSome = insertAt
-- :}
-- ...
-- ... insertAt: You must provide the index ...
-- ...
insertAtUnprovidedIndex = ()

#if __GLASGOW_HASKELL__ < 902

--------------------------------------------------------------------------------
-- |
-- >>> :{
-- insertSome :: Sem (e1 : e2 : e3 : e4 : r) a -> Sem (e1 : e2 : Reader i : e3 : e4 : r) a
-- insertSome = insertAt @1
-- :}
-- ...
-- ...insertAt: Failed to insert effects at index 1
-- ...
-- ... before ...
-- ...'[e1]
-- ... after ...
-- ...e2 : e3 : e4 : r
-- ...Actual ...
-- ...e1 : e2 : Reader i : e3 : e4 : r
-- ...
insertAtWrongIndex = ()

--------------------------------------------------------------------------------
-- |
-- Note: The /Actual/ row doesn't match the return type because of the 'raiseUnder'.
-- >>> :{
-- insertSome :: Sem (e1 : r) a -> Sem (e1 : e2 : State s : e3 : Reader i : e4 : r) a
-- insertSome = raiseUnder . insertAt @2 . raise2Under
-- :}
-- ...
-- ...insertAt: Failed to insert effects at index 2
-- ...
-- ... before ...
-- ...'[e1, State s]
-- ... after ...
-- ...e30 : r0
-- ...Actual ...
-- ...e1 : State s : e3 : Reader i : e4 : r
-- ...
insertAtAndRaiseWrongIndex = ()

#endif
