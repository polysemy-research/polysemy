{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}

module Polysemy.Internal.NonDet where

data NonDet m a
  = Empty
  | Choose (Bool -> a)

