-- | Description: Polysemy is a library for writing high-power, low-boilerplate domain specific languages
module Polysemy
  ( -- * Core Types
    Sem ()
  , Member
  , Members

    -- * Running Sem
  , run
  , runM

    -- * Type synonyms for user convenience
  , InterpreterFor
  , InterpretersFor

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
  , raise2Under
  , raise3Under
  , raiseUnder2
  , raiseUnder3
  , raise_
  , subsume_
  , insertAt

    -- * Simple Useful Combinators for Interpreting Effects
  , subsume
  , rewrite
  , transform
  , expose

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
    -- Each of these generated definitions make use of 'send' in order to perform
    -- the corresponding action of the effect. If you don't want to use
    -- Template Haskell, you can write the necessary boilerplate yourself by
    -- using 'send' directly.
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
  , send
  , makeSem
  , makeSem_

    -- * Combinators for Interpreting First-Order Effects
  , interpret
  , intercept
  , reinterpret
  , reinterpret2
  , reinterpret3

    -- * Combinators for Interpreting Higher-Order Effects
  , EffHandlerH
  , interpretH
  , interceptH
  , reinterpretH
  , reinterpret2H
  , reinterpret3H

    -- * Kind Synonyms
  , Effect
  , EffectRow

    -- * 'Handling'
    -- | When interpreting higher-order effects using 'interpretH' and friends,
    -- you must execute higher-order \"chunks\" given by the interpreted effect
    -- using 'runH' or 'runH''.
    --
    -- 'runH' and 'runH'' are enough for most purposes when using
    -- 'interpretH'. However, "Polysemy.Interpret" offers additional, more
    -- complicated features which are occasionally needed.
  , Handling
  , runH
  , runH'
  ) where

import Polysemy.Final
import Polysemy.Internal
import Polysemy.Internal.Combinators
import Polysemy.Internal.Interpret
import Polysemy.Internal.Kind
import Polysemy.Internal.TH.Effect
