{-# LANGUAGE AllowAmbiguousTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The meta-effect 'Scoped'
module Polysemy.Internal.Scoped where

import Data.Kind (Type)

import Data.Coerce
import Data.Functor.Identity
import Data.Functor.Const
import Polysemy
import Polysemy.Internal.Utils

-- | @Scoped@ is an effect for providing an effect interpreter for use within
-- application code, which can be accessed through 'scoped'. @Scoped@ is both
-- powerful and extremely versatile: 'Polysemy.Exceptional.Exceptional' is
-- defined in terms of 'Scoped', and the various @-Manager@ effects of the
-- library are defined in terms of 'Scoped1'.
--
-- A particularly notable use-case of 'Scoped' is resource acquisition:
-- as the interpreter is free to perform arbitrary actions before and after the
-- computation with the effect it is required to interpret, it can acquire a
-- resource, use that resource in order to interpret the effect and run the
-- computation, and then release the resource once the computation is
-- completed.
--
-- An application for this is @Polysemy.Conc.Events@ from
-- <https://hackage.haskell.org/package/polysemy-conc>, in which each program
-- using the effect @Polysemy.Conc.Consume@ is interpreted with its own copy of
-- the event channel; or a database transaction, in which a transaction handle
-- is created for the wrapped program and passed to the interpreter for the
-- database effect.
--
-- For a longer exposition, see <https://www.tweag.io/blog/2022-01-05-polysemy-scoped/>.
-- Note that the interface has changed dramatically since the blog post was
-- published: originally, the focus of 'Scoped' was exclusively the resource
-- acquisition aspect, but has been expanded as an effect for arbitrary
-- abstract effect interpretation.
--
-- The constructors of 'Scoped' and 'Scoped1' are not exposed; instead, 'Scoped'
-- is used through 'scoped', which is used like a local for @effect@.
-- 'scoped' takes an argument of type @param@, which will be passed through to
-- the interpreter. The @modifier@ parameter allows the interpreter to modify
-- the return value of the computation. 'Scoped_' is a type synonym for the
-- common case where the interpreter doesn't modify the return value.
--
-- As an example of the use of 'Scoped' for resource acquisition, imagine an
-- effect for writing lines to a file:
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
-- > prog :: Member ('Scoped_' FilePath Write) r => Sem r ()
-- > prog = do
-- >   scoped_ "file1.txt" do
-- >     write "line 1"
-- >     write "line 2"
-- >   scoped_ "file2.txt" do
-- >     write "line 1"
-- >     write "line 2"
--
-- Here 'scoped_' creates a prompt for an interpreter to start allocating a
-- resource for @"file1.txt"@ and handling @Write@ actions using that resource.
-- When the 'scoped_' block ends, the resource should be freed.
--
-- The interpreter may look like this:
--
-- > interpretWriteFile :: Members '[Bracket, Embed IO] => InterpreterFor (Scoped FilePath Write) r
-- > interpretWriteFile = runScoped_ $ \fp sem -> do
-- >   bracket (embed $ openFile fp WriteMode) (embed . hClose) $ \handle -> do
-- >     sem
-- >      & interpret \case
-- >        Write text -> embed $ Text.hPutStrLn fileHandle line
--
-- Essentially, the @bracket@ is executed at the point where @scoped@ was
-- called, wrapping the block that @scoped@ encompasses.
-- When the second @scoped@ is executed, another call to @bracket@ is performed.
--
-- The effect of this is that the operation that uses @Embed IO@ was moved from
-- the call site to the interpreter, while the interpreter may be executed at
-- the outermost layer of the app.
--
-- This makes it possible to use a pure interpreter for testing:
--
-- > interpretWriteOutput :: Member (Output (FilePath, Text)) r => InterpreterFor (Scoped FilePath Write) r
-- > interpretWriteOutput = runScoped_ $ \fp -> interpret \case
-- >   Write line -> output (fp, line)
newtype Scoped eff param modifier m a = Scoped (
    Scoped1 (Const2 eff :: () -> Effect)
            (Const param :: () -> Type)
            (Const1 modifier) m a
    )

-- | A convenience alias for 'Scoped' where the interpreter doesn't modify the
-- return type.
type Scoped_ eff param = Scoped eff param Identity

data Scoped1 (eff :: k -> Effect)
             (param :: k -> Type)
             (modifier :: k -> Type -> Type) :: Effect where
  RunInScope1 ::
      forall k eff param modifier m a
    . Word -> eff k m a -> Scoped1 eff param modifier m a
  Scoped1
    :: forall k eff param modifier m a b
     . (modifier k a -> b)
    -> param k -> (Word -> m a) -> Scoped1 eff param modifier m b

type Scoped1_ eff param = Scoped1 eff param (Const1 Identity)

-- | Constructor for @'Scoped' eff@: it takes a nested program which uses the
-- effect @eff@ and runs the interpreter provided by @'Scoped' eff@ on it, which
-- modifies the return type from @a@ to @modifier a@.
--
-- Please consult the documentation of 'Scoped' for details and examples.
scoped :: forall eff param modifier r a
         . Member (Scoped eff param modifier) r
        => param
        -> Sem (eff ': r) a
        -> Sem r (modifier a)
scoped param eff = send $
  Scoped $ Scoped1 @_ @(Const2 eff) @_ @(Const1 modifier)
                   getConst1 (Const param) $ \w ->
    transform
      (\e -> Scoped $
        RunInScope1 @_ @_ @(Const param) @(Const1 modifier) w (Const2 e))
      eff

-- | Constructor for @'Scoped_' eff@: it takes a nested program which uses the
--- effect @eff@ and runs the interpreter provided by @'Scoped_' eff@ on it.
--
-- Please consult the documentation of 'Scoped' for details and examples.
scoped_ :: forall eff param r a
         . Member (Scoped_ eff param) r
        => param
        -> Sem (eff ': r) a
        -> Sem r a
scoped_ p = fmap runIdentity . scoped @eff @param @Identity p

-- | Constructor for @'Scoped1' eff@: it takes a nested program which uses the
-- effect @eff k@ and runs the interpreter provided by @'Scoped1' eff@ on it,
-- which modifies the return type from @a@ to @modifier k a@.
--
-- Please consult the documentation of 'Scoped1' for details and examples.
scoped1 :: forall k eff param modifier r a
         . Member (Scoped1 eff param modifier) r
        => param k
        -> Sem (eff k ': r) a
        -> Sem r (modifier k a)
scoped1 param eff = send $ Scoped1 @_ @eff id param $ \w ->
  transform (RunInScope1 @_ @_ @param @modifier w) eff

-- | Constructor for @'Scoped1_' eff@: it takes a nested program which uses the
-- effect @eff k@ and runs the interpreter provided by @'Scoped1_' eff@ on it.
--
-- Please consult the documentation of 'Scoped1' for details and examples.
scoped1_ :: forall k eff param r a
          . Member (Scoped1_ eff param) r
         => param k
         -> Sem (eff k ': r) a
         -> Sem r a
scoped1_ p =
    fmap (runIdentity #. getConst1)
  . scoped1 @_ @eff @param @(Const1 Identity) p

-- | An auxiliary effect for 'Scoped' and 'Scoped1'.
data OuterRun (effect :: k -> Effect) :: Effect where
  OuterRun :: ∀ k effect m a . Word -> effect k m a -> OuterRun effect m a

-- | Transform the parameters and return value modifiers of a 'Scoped' program.
--
-- This allows incremental additions to the data passed to the interpreter, for
-- example to create an API that permits different ways of running an effect
-- with some fundamental parameters being supplied at scope creation and some
-- optional or specific parameters being selected by the user downstream.
rescope ::
  ∀ param0 modifier0 param1 modifier1 effect r .
  Member (Scoped effect param1 modifier1) r =>
  (param0 -> param1) ->
  (forall x. modifier1 x -> modifier0 x) ->
  InterpreterFor (Scoped effect param0 modifier0) r
rescope fp fm =
    transform (Scoped @effect @param1 @modifier1)
  . rescope1 @(Const param0 :: () -> Type)
             @(Const1 modifier0)
             @(Const param1)
             @(Const1 modifier1)
             @(Const2 effect)
             (coerce fp)
             (\(Const1 m) -> Const1 (fm m))
  . raiseUnder
  . coerceEffs
{-# inline rescope #-}

-- | Transform the parameters of a 'Scoped_' program.
rescope_ ::
  ∀ param0 param1 effect r .
  Member (Scoped_ effect param1) r =>
  (param0 -> param1) ->
  InterpreterFor (Scoped_ effect param0) r
rescope_ fp = rescope fp id
{-# inline rescope_ #-}

-- | Transform the parameters and return value modifiers of a 'Scoped1' program.
--
-- This allows incremental additions to the data passed to the interpreter, for
-- example to create an API that permits different ways of running an effect
-- with some fundamental parameters being supplied at scope creation and some
-- optional or specific parameters being selected by the user downstream.
rescope1 ::
  ∀ param0 modifier0 param1 modifier1 effect r .
  Member (Scoped1 effect param1 modifier1) r =>
  (forall k. param0 k -> param1 k) ->
  (forall k x. modifier1 k x -> modifier0 k x) ->
  InterpreterFor (Scoped1 effect param0 modifier0) r
rescope1 fp fm =
  transform \case
    RunInScope1 w e   -> RunInScope1 w e
    Scoped1 ex p main -> Scoped1 (ex . fm) (fp p) main
{-# inline rescope1 #-}

rescope1_ ::
  ∀ param0 param1 effect r .
  Member (Scoped1_ effect param1) r =>
  (forall k. param0 k -> param1 k) ->
  InterpreterFor (Scoped1_ effect param0) r
rescope1_ fp = rescope1 fp id
{-# inline rescope1_ #-}
