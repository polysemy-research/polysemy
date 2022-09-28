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
-- An application for this is @Polysemy.Conc.Events@ from <https://hackage.haskell.org/package/polysemy-conc>,
-- in which each program using the effect @Polysemy.Conc.Consume@ is interpreted with its own copy of the event channel;
-- or a database transaction, in which a transaction handle is created for the wrapped program and passed
-- to the interpreter for the database effect.
--
-- For a longer exposition, see <https://www.tweag.io/blog/2022-01-05-polysemy-scoped/>.
--
-- Resource creation is performed by the function passed to 'Polysemy.Scoped.runScoped'.
--
-- The constructors are not intended to be used directly; the smart constructor 'scoped' is used like a local
-- interpreter for @effect@.
-- 'scoped' takes an argument of type @param@, which will be passed through to the interpreter, to be used by the
-- resource allocation function.
data Scoped (param :: Type) (resource :: Type) (effect :: Effect) :: Effect where
  Run :: ∀ param resource effect m a . resource -> effect m a -> Scoped param resource effect m a
  InScope :: ∀ param resource effect m a . param -> (resource -> m a) -> Scoped param resource effect m a

-- |A convenience alias for a scope without parameters.
type Scoped_ resource effect =
  Scoped () resource effect

-- |Constructor for 'Scoped', taking a nested program and transforming all instances of @effect@ to
-- @'Scoped' param resource effect@ and wrapping the result with 'InScope'.
--
-- This allows the effective interpreter to bracket the nested program with a resource from a distance.
--
-- The value @param@ is passed to the interpreter.
scoped ::
  ∀ param resource effect r .
  Member (Scoped param resource effect) r =>
  param ->
  InterpreterFor effect r
scoped param main =
  send $ InScope @param @resource @effect param \ resource ->
    transform @effect (Run @param resource) main
{-# inline scoped #-}

-- |Constructor for 'Scoped_', taking a nested program and transforming all instances of @effect@ to
-- @'Scoped_' resource effect@ and wrapping the result with 'InScope'.
--
-- This allows the effective interpreter to bracket the nested program with a resource from a distance.
scoped_ ::
  ∀ resource effect r .
  Member (Scoped_ resource effect) r =>
  InterpreterFor effect r
scoped_ =
  scoped @() @resource ()
{-# inline scoped_ #-}

-- |Transform the parameters of a 'Scoped' program.
--
-- This allows incremental additions to the data passed to the interpreter, for example to create an API that permits
-- different ways of running an effect with some fundamental parameters being supplied at scope creation and some
-- optional or specific parameters being selected by the user downstream.
rescope ::
  ∀ param0 param1 resource effect r .
  Member (Scoped param1 resource effect) r =>
  (param0 -> param1) ->
  InterpreterFor (Scoped param0 resource effect) r
rescope fp =
  transform \case
    Run res e ->
      Run @param1 res e
    InScope p main ->
      InScope (fp p) main
{-# inline rescope #-}

-- |Helper for @runScoped@.
interpretWeaving ::
  ∀ e r .
  (∀ x . Weaving e (Sem (e : r)) x -> Sem r x) ->
  InterpreterFor e r
interpretWeaving h (Sem m) =
  Sem \ k -> m $ decomp >>> \case
    Right wav -> runSem (h wav) k
    Left g -> k $ hoist (interpretWeaving h) g
{-# inline interpretWeaving #-}
