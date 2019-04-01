{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fwarn-all-missed-specializations #-}

module Wtf where

import Polysemy
import Polysemy.State
import Polysemy.Effect.New


slowBeforeSpecialization :: Member (State Int) r => Semantic r Int
slowBeforeSpecialization = do
  n <- get
  if n <= 0
     then pure n
     else do
       put $ n - 1
       slowBeforeSpecialization

{-# SPECIALIZE slowBeforeSpecialization :: Semantic '[State Int] Int #-}


countDown :: Int -> Int
countDown start =
  fst . run . runState start . reinterpret send $ slowBeforeSpecialization

