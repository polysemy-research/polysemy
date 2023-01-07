module Polysemy.Error.Manager (
  -- * Effect
  ErrorManager,

  -- * Actions
  manageError,

  -- * Interpretations
  errorManagerToIOFinal,
  runErrorManager,
  customErrorManager,
  ) where

import Polysemy
import Polysemy.Error
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Membership
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data ErrorManagerMeta :: MetaEffect where
  ErrorManagerMeta :: z a
                   -> ErrorManagerMeta '[z :% Error e] m (Either e a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype ErrorManager m a = ErrorManager (Meta ErrorManagerMeta m a)

manageError :: Member ErrorManager r
            => Sem (Error e ': r) a -> Sem r (Either e a)
manageError m =
  ErrorManagerMeta (raiseUnder m)
  & sendMetaUsing Here
  & subsumeCoerce @ErrorManager

runErrorManager :: InterpreterFor ErrorManager r
runErrorManager = customErrorManager runError

errorManagerToIOFinal :: Member (Final IO) r
                      => InterpreterFor ErrorManager r
errorManagerToIOFinal = customErrorManager errorToIOFinal

customErrorManager :: (  forall e q x
                        . Sem (Error e ': Opaque q ': r) x
                       -> Sem (Opaque q ': r) (Either e x)
                       )
                   -> InterpreterFor ErrorManager r
customErrorManager interp =
  coerceEff
  >>> interpretMeta @ErrorManagerMeta \case
    ErrorManagerMeta m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp
      & runOpaqueBundleAt @0
