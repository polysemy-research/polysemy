{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}
module Wtf where

import Lib

countDown :: Int -> Int
countDown start = run $ runState start go
  where go = get >>= (\n -> if n <= 0 then (pure n) else (put (n-1)) >> go)

countDownIO :: Int -> IO Int
countDownIO start = runM $ runState start go
  where go = get >>= (\n -> if n <= 0 then (pure n) else (put (n-1)) >> go)
