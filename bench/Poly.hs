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


slowBeforeSpecialization :: Member (State Int) r => Sem r Int
slowBeforeSpecialization = do
  n <- get
  if n <= 0
     then pure n
     else do
       put $ n - 1
       slowBeforeSpecialization

{-# SPECIALIZE slowBeforeSpecialization :: Sem '[State Int] Int #-}


countDown :: Int -> Int
countDown s =
  fst . run . runState s $ slowBeforeSpecialization

prog
    :: Sem '[ State Bool
            , Error Bool
            , Resource
            , Embed IO
            ] Bool
prog = catch @Bool (throw True) (pure . not)

zoinks :: IO (Either Bool Bool)
zoinks = fmap (fmap snd)
       . (runM .@ lowerResource .@@ lowerError)
       . runState False
       $ prog

data Console m a where
  ReadLine :: Console m String
  WriteLine :: String -> Console m ()

makeSem ''Console

runConsoleBoring :: [String] -> Sem (Console ': r) a -> Sem r ([String], a)
runConsoleBoring inputs
  = runOutputMonoid (:[])
  . runInputList inputs
  . reinterpret2
  (\case
      ReadLine -> maybe "" id <$> input
      WriteLine msg -> output msg
  )

