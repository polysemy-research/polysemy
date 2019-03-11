{-# LANGUAGE DataKinds                      #-}

module Wtf where

import Lib
import Eff.Type
import Data.Functor.Identity

countDown :: Int -> Int
countDown start = fst $ run $ runState start go
  where
    go :: Eff '[State Int, Lift (Identity)] Int
    go = send Get >>= (\n -> if n <= 0 then (pure n) else (send $ Put (n-1)) >> go)



