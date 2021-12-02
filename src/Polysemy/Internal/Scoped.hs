{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Scoped where

import Polysemy.Internal (send, Member, InterpreterFor, Sem(Sem), runSem)
import Data.Kind (Type)
import Polysemy.Internal.Kind (Effect)
import Polysemy.Internal.Union (Weaving, hoist, decomp)
import Control.Arrow ((>>>))
import Polysemy.Internal.Combinators (transform)

-- |@Scoped@ transforms a program so that @effect@ is associated with a @resource@ within that program.
-- This requires the interpreter for @effect@ to be parameterized by @resource@ and constructed for every program using
-- @Scoped@ separately.
--
-- An application for this is 'Polysemy.Conc.Events', in which each program using the effect 'Polysemy.Conc.Consume' is
-- interpreted with its own copy of the event channel; or a database transaction, in which a transaction handle is
-- created for the wrapped program and passed to the interpreter for the database effect.
--
-- Resource creation is performed by the function passed to 'Polysemy.Conc.Interpreter.runScoped'.
--
-- The constructors are not intended to be used directly; the smart constructor 'scoped' is used like a local
-- interpreter for @effect@.
data Scoped (resource :: Type) (effect :: Effect) :: Effect where
  Run :: ∀ resource effect m a . resource -> effect m a -> Scoped resource effect m a
  InScope :: ∀ resource effect m a . (resource -> m a) -> Scoped resource effect m a

-- |Constructor for 'Scoped', taking a nested program and transforming all instances of @effect@ to
-- @Scoped resource effect@.
scoped ::
  ∀ resource effect r .
  Member (Scoped resource effect) r =>
  InterpreterFor effect r
scoped main =
  send $ InScope @resource @effect \ resource ->
    transform @effect (Run resource) main

-- |Helper for @runScoped@.
interpretH' ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretH' h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k $ hoist (interpretH' h) g
