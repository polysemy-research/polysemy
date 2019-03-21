{-# LANGUAGE BangPatterns #-}

module Main where

import Wtf

main :: IO ()
main = let !_ = countDown 100000000 in pure ()
