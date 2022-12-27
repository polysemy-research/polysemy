{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Scoped where

import Data.Kind (Type)

import Polysemy

-- | @Scoped@ transforms a program so that an interpreter for @effect@ may
-- perform arbitrary actions, like resource management, before and after the
-- computation wrapped by a call to 'scoped' is executed.
--
-- An application for this is @Polysemy.Conc.Events@ from
-- <https://hackage.haskell.org/package/polysemy-conc>, in which each program
-- using the effect @Polysemy.Conc.Consume@ is interpreted with its own copy of
-- the event channel; or a database transaction, in which a transaction handle
-- is created for the wrapped program and passed to the interpreter for the
-- database effect.
--
-- For a longer exposition, see <https://www.tweag.io/blog/2022-01-05-polysemy-scoped/>.
-- Note that the interface has changed since the blog post was published: The
-- @resource@ parameter no longer exists.
--
-- Resource allocation is performed by a function passed to
-- 'Polysemy.Scoped.interpretScoped'.
--
-- The constructors are not intended to be used directly; the smart constructor
-- 'scoped' is used like a local interpreter for @effect@. 'scoped' takes an
-- argument of type @param@, which will be passed through to the interpreter, to
-- be used by the resource allocation function.
--
-- As an example, imagine an effect for writing lines to a file:
--
-- > data Write :: Effect where
-- >   Write :: Text -> Write m ()
-- > makeSem ''Write
--
-- If we now have the following requirements:
--
-- 1. The file should be opened and closed right before and after the part of
--    the program in which we write lines
-- 2. The file name should be specifiable at the point in the program where
--    writing begins
-- 3. We don't want to commit to IO, lines should be stored in memory when
--    running tests
--
-- Then we can take advantage of 'Scoped' to write this program:
--
-- > prog :: Member (Scoped FilePath Write) r => Sem r ()
-- > prog = do
-- >   scoped "file1.txt" do
-- >     write "line 1"
-- >     write "line 2"
-- >   scoped "file2.txt" do
-- >     write "line 1"
-- >     write "line 2"
--
-- Here 'scoped' creates a prompt for an interpreter to start allocating a
-- resource for @"file1.txt"@ and handling @Write@ actions using that resource.
-- When the 'scoped' block ends, the resource should be freed.
--
-- The interpreter may look like this:
--
-- > interpretWriteFile :: Members '[Resource, Embed IO] => InterpreterFor (Scoped FilePath Write) r
-- > interpretWriteFile =
-- >   interpretScoped allocator handler
-- >   where
-- >     allocator name use = bracket (openFile name WriteMode) hClose use
-- >     handler fileHandle (Write line) = embed (Text.hPutStrLn fileHandle line)
--
-- Essentially, the @bracket@ is executed at the point where @scoped@ was
-- called, wrapping the following block. When the second @scoped@ is executed,
-- another call to @bracket@ is performed.
--
-- The effect of this is that the operation that uses @Embed IO@ was moved from
-- the call site to the interpreter, while the interpreter may be executed at
-- the outermost layer of the app.
--
-- This makes it possible to use a pure interpreter for testing:
--
-- > interpretWriteOutput :: Member (Output (FilePath, Text)) r => InterpreterFor (Scoped FilePath Write) r
-- > interpretWriteOutput =
-- >   interpretScoped (\ name use -> use name) \ name -> \case
-- >     Write line -> output (name, line)
--
-- Here we simply pass the name to the interpreter in the resource allocation
-- function.
--
-- Now imagine that we drop requirement 2 from the initial list – we still want
-- the file to be opened and closed as late/early as possible, but the file name
-- is globally fixed. For this case, the @param@ type is unused, and the API
-- provides some convenience aliases to make your code more concise:
--
-- > prog :: Member (Scoped_ Write) r => Sem r ()
-- > prog = do
-- >   scoped_ do
-- >     write "line 1"
-- >     write "line 2"
-- >   scoped_ do
-- >     write "line 1"
-- >     write "line 2"
--
-- The type 'Scoped_' and the constructor 'scoped_' simply fix @param@ to @()@.
data Scoped (param :: Type) (effect :: Effect) :: Effect where
  Run :: ∀ param effect m a . Word -> effect m a -> Scoped param effect m a
  InScope :: ∀ param effect m a . param -> (Word -> m a) -> Scoped param effect m a

data OuterRun (effect :: Effect) :: Effect where
  OuterRun :: ∀ effect m a . Word -> effect m a -> OuterRun effect m a

-- |A convenience alias for a scope without parameters.
type Scoped_ effect =
  Scoped () effect

-- | Constructor for 'Scoped', taking a nested program and transforming all
-- instances of @effect@ to @'Scoped' param effect@.
--
-- Please consult the documentation of 'Scoped' for details and examples.
scoped ::
  ∀ param effect r .
  Member (Scoped param effect) r =>
  param ->
  InterpreterFor effect r
scoped param main =
  send $ InScope @param @effect param $ \w ->
    transform @effect (Run @param w) main
{-# inline scoped #-}

-- | Constructor for 'Scoped_', taking a nested program and transforming all
-- instances of @effect@ to @'Scoped_' effect@.
--
-- Please consult the documentation of 'Scoped' for details and examples.
scoped_ ::
  ∀ effect r .
  Member (Scoped_ effect) r =>
  InterpreterFor effect r
scoped_ = scoped ()
{-# inline scoped_ #-}

-- | Transform the parameters of a 'Scoped' program.
--
-- This allows incremental additions to the data passed to the interpreter, for
-- example to create an API that permits different ways of running an effect
-- with some fundamental parameters being supplied at scope creation and some
-- optional or specific parameters being selected by the user downstream.
rescope ::
  ∀ param0 param1 effect r .
  Member (Scoped param1 effect) r =>
  (param0 -> param1) ->
  InterpreterFor (Scoped param0 effect) r
rescope fp =
  transform \case
    Run w e        -> Run @param1 w e
    InScope p main -> InScope (fp p) main
{-# inline rescope #-}

