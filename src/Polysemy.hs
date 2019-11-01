module Polysemy
  ( -- * Core Types
    Sem ()
  , Member
  , MemberWithError
  , Members

  -- * Running Sem
  , run
  , runM
  , runFinal

  -- * Type synonyms for user convenience
  , InterpreterFor

  -- * Interoperating With Other Monads
  -- ** Embed
  , Embed (..)
  , embed
  , embedToFinal

  -- ** Final
  -- | For advanced uses of 'Final', including creating your own interpreters
  -- that make use of it, see "Polysemy.Final"
  , Final
  , embedFinal

    -- * Lifting
  , raise
  , raiseUnder
  , raiseUnder2
  , raiseUnder3

    -- * Trivial Interpretation
  , subsume

    -- * Creating New Effects
    -- | Effects should be defined as a GADT (enable @-XGADTs@), with kind @(*
    -- -> *) -> * -> *@. Every primitive action in the effect should be its
    -- own constructor of the type. For example, we can model an effect which
    -- interacts with a tty console as follows:
    --
    -- @
    -- data Console m a where
    --   WriteLine :: String -> Console m ()
    --   ReadLine  :: Console m String
    -- @
    --
    -- Notice that the @a@ parameter gets instantiated at the /desired return/
    -- /type/ of the actions. Writing a line returns a @()@, but reading one
    -- returns 'String'.
    --
    -- By enabling @-XTemplateHaskell@, we can use the 'makeSem' function
    -- to generate smart constructors for the actions. These smart constructors
    -- can be invoked directly inside of the 'Sem' monad.
    --
    -- > makeSem ''Console
    --
    -- results in the following definitions:
    --
    -- @
    -- writeLine :: 'Member' Console r => String -> 'Sem' r ()
    -- readLine  :: 'Member' Console r => 'Sem' r String
    -- @
    --
    -- Effects which don't make use of the @m@ parameter are known as
    -- "first-order effects."

    -- ** Higher-Order Effects
    -- | Every effect has access to the @m@ parameter, which corresponds to the
    -- 'Sem' monad it's used in. Using this parameter, we're capable of
    -- writing effects which themselves contain subcomputations.
    --
    -- For example, the definition of 'Polysemy.Error.Error' is
    --
    -- @
    -- data 'Polysemy.Error.Error' e m a where
    --   'Polysemy.Error.Throw' :: e -> 'Polysemy.Error.Error' e m a
    --   'Polysemy.Error.Catch' :: m a -> (e -> m a) -> 'Polysemy.Error.Error' e m a
    -- @
    --
    -- where 'Polysemy.Error.Catch' is an action that can run an exception
    -- handler if its first argument calls 'Polysemy.Error.throw'.
    --
    -- > makeSem ''Error
    --
    -- @
    -- 'Polysemy.Error.throw' :: 'Member' ('Polysemy.Error.Error' e) r => e -> 'Sem' r a
    -- 'Polysemy.Error.catch'  :: 'Member' ('Polysemy.Error.Error' e) r => 'Sem' r a -> (e -> 'Sem' r a) -> 'Sem' r a
    -- @
    --
    -- As you see, in the smart constructors, the @m@ parameter has become @'Sem' r@.
  , makeSem
  , makeSem_

    -- * Combinators for Interpreting First-Order Effects
  , interpret
  , intercept
  , reinterpret
  , reinterpret2
  , reinterpret3
  , rewrite
  , transform

    -- * Combinators for Interpreting Higher-Order Effects
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
  , reinterpret3H

    -- * Combinators for Interpreting Directly to IO
  , withLowerToIO

    -- * Kind Synonyms
  , Effect
  , EffectRow

    -- * Composing IO-based Interpreters
  , (.@)
  , (.@@)

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
  , getInspectorT
  , Inspector (..)
  ) where

import Polysemy.Final
import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.Forklift
import Polysemy.Internal.Kind
import Polysemy.Internal.Tactics
import Polysemy.Internal.TH.Effect

