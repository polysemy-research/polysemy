{-# LANGUAGE DataKinds                      #-}

module Wtf where

import Lib
import Eff.Type
import Data.Functor.Identity

countDown :: Int -> Int
countDown start = fst $ run $ runState start go
  where
    go :: Eff '[State Int, Identity] Int
    go = get >>= (\n -> if n <= 0 then (pure n) else (put (n-1)) >> go)



