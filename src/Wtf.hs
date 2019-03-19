{-# LANGUAGE DataKinds                      #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}

module Wtf where

import Definitive
import TRYAGAIN

go :: Def '[State Int] Int
go = do
  n <- send (Get id)
  if n <= 0
     then pure n
     else do
       send $ Put (n-1) ()
       go

countDown :: Int -> Int
countDown start = fst $ run $ runState start $ reinterpret send $ go

