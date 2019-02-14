{-# LANGUAGE DataKinds                      #-}

module Wtf where

import Lib
import Eff.Type
import Data.Functor.Identity

countDown :: Int -> Int
countDown start = run $ runState "hello" $ runState start go
  where
    go :: Eff '[State Int, State String, Identity] Int
    go = get >>= (\n -> if n <= 0 then (pure n) else (put (n-1)) >> go)



