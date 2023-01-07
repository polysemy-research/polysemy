module Polysemy.Output.Manager (
  -- * Effect
  OutputManager,

  -- * Actions
  manageOutput,
  manageOutputAssocR,
  manageOutputList,

  -- * Interpretations
  outputManagerToIO,
  runOutputManager,
  customOutputManager,
  ) where

import Data.Bifunctor (first)
import Data.Semigroup
import Polysemy
import Polysemy.Output
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Membership
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data OutputManagerMeta :: MetaEffect where
  OutputManagerMeta :: Monoid o
                    => z a
                    -> OutputManagerMeta '[z :% Output o] m (o, a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype OutputManager m a = OutputManager (Meta OutputManagerMeta m a)

manageOutput :: (Monoid o, Member OutputManager r)
             => Sem (Output o ': r) a -> Sem r (o, a)
manageOutput =
  (OutputManagerMeta . raiseUnder)
  >>> sendMetaUsing Here
  >>> subsumeCoerce @OutputManager

manageOutputList :: Member OutputManager r
                 => Sem (Output o ': r) a -> Sem r ([o], a)
manageOutputList =
  rewrite (\(Output o) -> Output (Dual [o]))
  >>> manageOutput
  >>> (fmap . first) (reverse .# getDual)

manageOutputAssocR :: (Monoid o, Member OutputManager r)
                   => Sem (Output o ': r) a -> Sem r (o, a)
manageOutputAssocR =
  outputIntoEndoOutput
  >>> manageOutput
  >>> (fmap . first) (`appEndo` mempty)

runOutputManager :: InterpreterFor OutputManager r
runOutputManager = customOutputManager (runOutputMonoid id)

outputManagerToIO :: Member (Embed IO) r
                  => InterpreterFor OutputManager r
outputManagerToIO = customOutputManager (outputToIOMonoid id)

customOutputManager :: (  forall o q x
                        . Monoid o
                       => Sem (Output o ': Opaque q ': r) x
                       -> Sem (Opaque q ': r) (o, x)
                       )
                   -> InterpreterFor OutputManager r
customOutputManager interp =
  coerceEff
  >>> interpretMeta @OutputManagerMeta \case
    OutputManagerMeta m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp
      & runOpaqueBundleAt @0
