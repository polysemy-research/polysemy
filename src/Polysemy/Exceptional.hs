module Polysemy.Exceptional (
  -- * Effect
  Exceptional,
  Stop(..),

  -- * Actions
  exceptional,
  (Polysemy.Exceptional.!!),
  stop,

  -- * Interpretations
  runExceptional,
  ) where

import Polysemy
import Polysemy.Meta
import Polysemy.Fatal
import Polysemy.Membership
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data ExceptionalMeta eff exc :: MetaEffect where
  ExceptionalMeta :: z a
                  -> ExceptionalMeta eff exc '[z :% eff] m (Either exc a)

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype Exceptional eff exc m a =
  Exceptional (Meta (ExceptionalMeta eff exc) m a)

newtype Stop e m a where
  Stop :: e -> Stop e m void

stop :: Member (Stop e) r => e -> Sem r void
stop = send .# Stop

exceptional :: forall eff exc r a
             . Member (Exceptional eff exc) r
            => Sem (eff ': r) a -> (exc -> Sem r a) -> Sem r a
exceptional m h =
  ExceptionalMeta (raiseUnder m)
  & sendMetaUsing Here
  & subsumeCoerce @(Exceptional eff exc)
  >>= either h return

-- I wonder how this is usable considering you can't use type applications.
-- Feels like ambiguity issues would bite you in the arse. polysemy-plugin,
-- probably. Deserves documentation if so.
-- TODO: infix?
(!!) :: forall eff exc r a
      . Member (Exceptional eff exc) r
     => Sem (eff ': r) a -> (exc -> Sem r a) -> Sem r a
(!!) = exceptional

-- TODO: require the interpreter to be a reinterpreter (current) or a regular
-- interpreter? Equivalent in strength, only a question of convenience
runExceptional :: forall eff exc r
                . (   forall q x
                    . Sem (eff ': Opaque q ': r) x
                   -> Sem (Stop exc ': Opaque q ': r) x
                  )
               -> InterpreterFor (Exceptional eff exc) r
runExceptional interp =
  coerceEff
  >>> interpretMeta @(ExceptionalMeta eff exc) \case
    ExceptionalMeta m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp
      & coerceEff
      & runFatal
      & runOpaqueBundleAt @0
