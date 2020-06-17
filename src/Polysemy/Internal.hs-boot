{-# LANGUAGE RoleAnnotations #-}
module Polysemy.Internal where

import Polysemy.Internal.Kind
import Data.Kind (Type)

type role Sem nominal nominal
data Sem (r :: EffectRow) (a :: Type)

instance Functor (Sem r)
instance Applicative (Sem r)
instance Monad (Sem r)
