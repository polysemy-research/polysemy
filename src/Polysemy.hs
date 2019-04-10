module Polysemy
  ( Semantic ()
  , Member
  -- * Running
  , run
  , runM
  , (.@)
  , (.@@)
  -- * Monads
  , Lift ()
  , sendM
    -- * Raising
  , raise
    -- * TH
  , makeSemantic
  , makeSemantic_
    -- * First order
  , interpret
  , intercept
  , reinterpret
  , reinterpret2
  , reinterpret3
    -- * Higher order
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
  , reinterpret3H
    -- * Statefulness
  , stateful
  , lazilyStateful
    -- * Performance
  , inlineRecursiveCalls

    -- * Tactics
    -- | Higher-order effects need to explicitly thread /other effects'/ state
    -- through themselves. Tactics are a domain-specific language for describing
    -- exactly how this threading should take place.
    --
    -- The first computation to be run should use 'runT', and subsequent
    -- computations /in the same environment/ should use 'bindT'. Any
    -- first-order constructors which appear in a higher-order context may use
    -- 'pureT' to satisfy the typechecker.
  , Tactical
  , WithTactics
  , getInitialStateT
  , pureT
  , runT
  , bindT

  -- * Reexports
  , Typeable
  ) where

import Data.Typeable
import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.TH.Effect
import Polysemy.Internal.TH.Performance
import Polysemy.Internal.Tactics

