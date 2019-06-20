{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NoPolyKinds    #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.NonDet where

import Data.Kind


------------------------------------------------------------------------------
-- | An effect corresponding to the 'Control.Applicative.Alternative' typeclass.
data NonDet (m :: Type -> Type) a
  = Empty
  | Choose (Bool -> a)

