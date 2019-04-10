{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NoPolyKinds    #-}

module Polysemy.Internal.NonDet where

------------------------------------------------------------------------------
-- | An effect corresponding to the 'Control.Applicative.Alternative' typeclass.
data NonDet (m :: * -> *) a
  = Empty
  | Choose (Bool -> a)

