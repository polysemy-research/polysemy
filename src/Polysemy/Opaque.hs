-- | The auxiliary effect 'Opaque' used by interpreters of 'Polysemy.Scoped.Scoped'
module Polysemy.Opaque (
  -- * Effect
  Opaque(..),

  -- * Interpreters
  toOpaque,
  fromOpaque,
  ) where

import Polysemy
import Polysemy.Internal.Opaque
import Polysemy.Internal.Utils

-- | Wrap 'Opaque' around the top effect of the effect stack
toOpaque :: Sem (e ': r) a -> Sem (Opaque e ': r) a
toOpaque = coerceEffs
{-# INLINE toOpaque #-}

-- | Unwrap 'Opaque' around the top effect of the effect stack
fromOpaque :: Sem (Opaque e ': r) a -> Sem (e ': r) a
fromOpaque = coerceEffs
{-# INLINE fromOpaque #-}
