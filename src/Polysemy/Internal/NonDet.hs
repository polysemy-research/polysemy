{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NoPolyKinds    #-}

module Polysemy.Internal.NonDet where

data NonDet (m :: * -> *) a
  = Empty
  | Choose (Bool -> a)

