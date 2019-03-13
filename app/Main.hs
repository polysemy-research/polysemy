{-# LANGUAGE BangPatterns                   #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-all #-}

module Main where

import Wtf
import Control.Monad

main :: IO ()
main = let !x = countDown 100000000 in pure ()
