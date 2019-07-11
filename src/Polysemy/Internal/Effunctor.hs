module Polysemy.Internal.Effunctor where

import Data.Kind
import Polysemy.Internal
import Polysemy.Internal.Kind

-- | Effects that are \"functors\" over their argument.
--
-- TODO: add better docs
class Effunctor (e1 :: Type -> Effect) where
  effmap
    :: forall r a b c
     . Member (e1 b) r
    => (a -> b)
    -> Sem (e1 a ': r) c
    -> Sem r c
