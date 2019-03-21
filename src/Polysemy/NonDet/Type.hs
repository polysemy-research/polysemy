{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

module Polysemy.NonDet.Type where

import Polysemy.Effect

data NonDet m a
  = Empty
  | Choose (Bool -> a)
  deriving (Functor, Effect)

