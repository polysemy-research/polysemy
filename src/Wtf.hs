{-# LANGUAGE DataKinds                      #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}

module Wtf where

import TRYAGAIN
import Data.Functor.Identity

countDown :: Int -> Int
countDown start = fst $ run $ runState go start
  where
    go :: Eff '[State Int, Lift (Identity)] Int
    go = do
      n <- send (Get id)
      if n <= 0
         then pure n
         else do
           send $ Put (n-1) ()
           go



