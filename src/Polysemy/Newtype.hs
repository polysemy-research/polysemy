{-# LANGUAGE AllowAmbiguousTypes, QuantifiedConstraints #-}
module Polysemy.Newtype where

import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union
import Data.Coerce

-- wrapEffect :: forall nT e r a
--             . Member nT r
--            => Sem (e ': r) a
--            -> Sem r a
-- wrapEffect = hoistSem $ \u -> hoist (wrapEffect @nT @e) $ case decomp u
