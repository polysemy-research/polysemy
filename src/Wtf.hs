{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Wtf where

import Polysemy
import Polysemy.State

go :: Member (State Int) r => Semantic r Int
go = do
  n <- get
  if n <= 0
     then pure n
     else do
       put $ n - 1
       go


countDown :: Int -> Int
countDown start =
  fst . run . runState start . reinterpret send $ go

