{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -fwarn-all-missed-specializations #-}

module Poly where

import Polysemy
import Polysemy.Error
import Polysemy.Resource
import Polysemy.State
import Polysemy.Input
import Polysemy.Output


slowBeforeSpecialization :: Member (State Int) r => Semantic r Int
slowBeforeSpecialization = do
  n <- get
  if n <= 0
     then pure n
     else do
       put $ n - 1
       slowBeforeSpecialization

{-# SPECIALIZE slowBeforeSpecialization :: Semantic '[State Int] Int #-}


countDown :: Int -> Int
countDown s =
  fst . run . runState s $ slowBeforeSpecialization

prog
    :: Semantic '[ State Bool
                 , Error Bool
                 , Resource
                 , Lift IO
                 ] Bool
prog = catch @Bool (throw True) (pure . not)

zoinks :: IO (Either Bool Bool)
zoinks = fmap (fmap snd)
       . (runM .@ runResource .@@ runErrorInIO)
       . runState False
       $ prog

data Console m a where
  ReadLine :: Console m String
  WriteLine :: String -> Console m ()

makeSemantic ''Console

runConsoleBoring :: [String] -> Semantic (Console ': r) a -> Semantic r ([String], a)
runConsoleBoring inputs
  = runFoldMapOutput (:[])
  . runListInput inputs
  . reinterpret2
  (\case
      ReadLine -> maybe "" id <$> input
      WriteLine msg -> output msg
  )

