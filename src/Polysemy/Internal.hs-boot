{-# LANGUAGE RoleAnnotations #-}
module Polysemy.Internal where

import Polysemy.Internal.Kind

type role Sem nominal nominal
data Sem (r :: EffectRow) (a :: *)

instance Functor (Sem r)
instance Applicative (Sem r)
instance Monad (Sem r)
