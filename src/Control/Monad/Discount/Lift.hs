{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia    #-}
{-# LANGUAGE KindSignatures #-}

module Control.Monad.Discount.Lift where

import Control.Monad.Discount.Effect
import Data.Coerce


newtype Lift m (z :: * -> *) a = Lift
  { unLift :: m a
  }
  deriving (Functor, Applicative, Monad) via m
  deriving Effect

