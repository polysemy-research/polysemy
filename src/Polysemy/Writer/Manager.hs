module Polysemy.Writer.Manager (
  -- * Effect
  WriterManager,

  -- * Actions
  manageWriter,
  manageWriterAssocR,

  -- * Interpretations
  writerManagerToIOFinal,
  runWriterManager,
  customWriterManager,
  ) where

import Data.Bifunctor (first)
import Data.Semigroup
import Polysemy
import Polysemy.Writer
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Membership
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data WriterManagerMeta :: MetaEffect where
  WriterManagerMeta :: Monoid o
                    => z a
                    -> WriterManagerMeta '[z :% Writer o] m (o, a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype WriterManager m a = WriterManager (Meta WriterManagerMeta m a)

manageWriter :: (Monoid o, Member WriterManager r)
             => Sem (Writer o ': r) a -> Sem r (o, a)
manageWriter =
  (WriterManagerMeta . raiseUnder)
  >>> sendMetaUsing Here
  >>> subsumeCoerce @WriterManager

manageWriterAssocR :: (Monoid o, Member WriterManager r)
                   => Sem (Writer o ': r) a -> Sem r (o, a)
manageWriterAssocR =
    (fmap . first) (`appEndo` mempty)
  . manageWriter
  . writerIntoEndoWriter

runWriterManager :: InterpreterFor WriterManager r
runWriterManager = customWriterManager runWriter

writerManagerToIOFinal :: Member (Final IO) r
                      => InterpreterFor WriterManager r
writerManagerToIOFinal = customWriterManager writerToIOFinal

customWriterManager :: (  forall o q x
                        . Monoid o
                       => Sem (Writer o ': Opaque q ': r) x
                       -> Sem (Opaque q ': r) (o, x)
                       )
                    -> InterpreterFor WriterManager r
customWriterManager interp =
  coerceEff
  >>> interpretMeta @WriterManagerMeta \case
    WriterManagerMeta m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp
      & runOpaqueBundleAt @0
