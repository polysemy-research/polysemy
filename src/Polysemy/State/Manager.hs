module Polysemy.State.Manager (
  -- * Effect
  StateManager,

  -- * Actions
  manageState,

  -- * Interpretations
  stateManagerToIO,
  runStateManager,
  customStateManager,
  ) where

import Polysemy
import Polysemy.State
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Membership
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data StateManagerMeta :: MetaEffect where
  StateManagerMeta :: s
                   -> z a
                   -> StateManagerMeta '[z :% State s] m (s, a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype StateManager m a = StateManager (Meta StateManagerMeta m a)

manageState :: Member StateManager r
            => s -> Sem (State s ': r) a -> Sem r (s, a)
manageState s m =
  StateManagerMeta s (raiseUnder m)
  & sendMetaUsing Here
  & subsumeCoerce @StateManager

runStateManager :: InterpreterFor StateManager r
runStateManager = customStateManager runState

stateManagerToIO :: Member (Embed IO) r
                 => InterpreterFor StateManager r
stateManagerToIO = customStateManager stateToIO

customStateManager :: (  forall s q x
                        . s
                       -> Sem (State s ': Opaque q ': r) x
                       -> Sem (Opaque q ': r) (s, x)
                       )
                   -> InterpreterFor StateManager r
customStateManager interp =
  coerceEff
  >>> interpretMeta @StateManagerMeta \case
    StateManagerMeta s m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp s
      & runOpaqueBundleAt @0
