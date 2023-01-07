module Polysemy.AtomicState.Manager (
  -- * Effect
  AtomicStateManager,

  -- * Actions
  manageAtomicState,

  -- * Interpretations
  atomicStateManagerToIO,
  runAtomicStateManagerViaState,
  customAtomicStateManager,
  ) where

import Polysemy
import Polysemy.AtomicState
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Membership
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data AtomicStateManagerMeta :: MetaEffect where
  AtomicStateManagerMeta
    :: s
    -> z a
    -> AtomicStateManagerMeta '[z :% AtomicState s] m (s, a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype AtomicStateManager m a =
  AtomicStateManager (Meta AtomicStateManagerMeta m a)

manageAtomicState :: Member AtomicStateManager r
                  => s -> Sem (AtomicState s ': r) a -> Sem r (s, a)
manageAtomicState s m =
  AtomicStateManagerMeta s (raiseUnder m)
  & sendMetaUsing Here
  & subsumeCoerce @AtomicStateManager

runAtomicStateManagerViaState :: InterpreterFor AtomicStateManager r
runAtomicStateManagerViaState = customAtomicStateManager runAtomicStateViaState

atomicStateManagerToIO :: Member (Embed IO) r
                       => InterpreterFor AtomicStateManager r
atomicStateManagerToIO = customAtomicStateManager atomicStateToIO

customAtomicStateManager :: (  forall s q x
                             . s
                            -> Sem (AtomicState s ': Opaque q ': r) x
                            -> Sem (Opaque q ': r) (s, x)
                            )
                         -> InterpreterFor AtomicStateManager r
customAtomicStateManager interp =
  coerceEff
  >>> interpretMeta @AtomicStateManagerMeta \case
    AtomicStateManagerMeta s m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp s
      & runOpaqueBundleAt @0
