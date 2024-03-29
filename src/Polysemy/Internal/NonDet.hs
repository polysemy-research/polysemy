{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE NoPolyKinds    #-}

{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The 'NonDet' effect
module Polysemy.Internal.NonDet where


------------------------------------------------------------------------------
-- | An effect corresponding to the 'Control.Applicative.Alternative' typeclass.
data NonDet m a
  = Empty
  | Choose (m a) (m a)

