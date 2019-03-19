{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeApplications #-}

module Wtf where

import Definitive
import Definitive.State

go :: Def '[State Int] Int
go = do
  n <- get
  if n <= 0
     then pure n
     else do
       put $ n - 1
       go


countDown :: Int -> Int
countDown start = fst $ run $ runState start $ reinterpret send $ go

