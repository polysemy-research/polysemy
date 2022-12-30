module Polysemy.Internal.Utils
  ((>>>), (<&>), (&),
   module Polysemy.Internal.Utils) where

import Control.Arrow ((>>>))
import Data.Functor ((<&>))
import Data.Function ((&))
import Data.Coerce
import Unsafe.Coerce
import Data.Kind (Type)
import Polysemy.Internal.Kind

newtype Const2 (e :: b -> c -> Type) (p :: k) (m :: b) (a :: c) =
  Const2 { getConst2 :: e m a }

newtype Const1 (m :: b -> Type) (p :: k) (a :: b) =
  Const1 { getConst1 :: m a }

coerceEffs :: forall r' r a (eff :: [Effect] -> Type -> Type)
            . Coercible r' r
           => eff r' a
           -> eff r a
coerceEffs = unsafeCoerce

infixr 9 #.
(#.) :: forall a b c. Coercible b c => (b -> c) -> (a -> b) -> (a -> c)
(#.) _ = coerce
{-# INLINE (#.) #-}

infixl 8 .#
(.#) :: forall a b c. Coercible a b => (b -> c) -> (a -> b) -> (a -> c)
(.#) bc _ = coerce bc
{-# INLINE (.#) #-}
