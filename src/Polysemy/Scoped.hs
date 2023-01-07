module Polysemy.Scoped (
  -- * Effect
  Scoped,
  Scoped_,

  -- * Actions
  scoped,
  scoped_,

  -- * Interpretations
  runScoped,
  runScoped_,
  ) where

import Polysemy
import Polysemy.Membership
import Polysemy.Meta
import Polysemy.Opaque
import Polysemy.Newtype
import Polysemy.Internal.Utils

-- TODO: consider exposing this. Either from here or an internal module
data ScopedMeta eff param :: MetaEffect where
  ScopedMeta :: param -> z a -> ScopedMeta eff param '[z :% eff] m a

-- TODO: consider exposing the constructor.
-- Either from here or an internal module
newtype Scoped eff param m a = Scoped (Meta (ScopedMeta eff param) m a)

type Scoped_ eff = Scoped eff ()

scoped :: forall eff param r a
        . Member (Scoped eff param) r
       => param -> Sem (eff ': r) a -> Sem r a
scoped p m =
  ScopedMeta p (raiseUnder m)
  & sendMetaUsing Here
  & subsumeCoerce @(Scoped eff param)

scoped_ :: Member (Scoped_ eff) r => Sem (eff ': r) a -> Sem r a
scoped_ = scoped ()
{-# INLINE scoped_ #-}

runScoped :: forall eff param r
           . (   forall q x
               . param
              -> Sem (eff ': Opaque q ': r) x -> Sem (Opaque q ': r) x
             )
          -> InterpreterFor (Scoped eff param) r
runScoped interp =
  coerceEff
  >>> interpretMeta @(ScopedMeta eff param) \case
    ScopedMeta param m ->
      runMeta m
      & collectOpaqueBundleAt @1 @'[_, _]
      & interp param
      & runOpaqueBundleAt @0

runScoped_
  :: (forall q x. Sem (eff ': Opaque q ': r) x -> Sem (Opaque q ': r) x)
  -> InterpreterFor (Scoped_ eff) r
runScoped_ interp = runScoped (\() -> interp)
