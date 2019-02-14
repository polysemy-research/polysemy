module Main where

import Wtf
import Control.Monad

main :: IO ()
main = replicateM_ 1000000 $ print $ countDown 10000
