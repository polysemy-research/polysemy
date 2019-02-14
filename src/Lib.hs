{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeOperators          #-}
{-# OPTIONS_GHC -Wall               #-}

module Lib where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import           Data.OpenUnion
import           Eff.Interpretation
import           Eff.Type


data State s a where
  Get :: State s s
  Put :: s -> State s ()


get :: Member (State s) r => Eff r s
get = send Get
{-# INLINE get #-}


put :: Member (State s) r => s -> Eff r ()
put = send . Put
{-# INLINE put #-}


foom :: Eff '[State String, IO] String
foom = do
  put "nice!"
  put "nice!"
  put "nice!"
  get @String


runTeletype :: Member IO r => Eff (State String ': r) a -> Eff r a
runTeletype = interpret $ \case
    Get   -> send getLine
    Put s -> send $ putStrLn s


-- main :: IO ()
-- main = runM (runState "fuck" foom) >>= print


runState :: s -> Eff (State s ': r) a -> Eff r a
runState = stateful $ \case
  Get    -> S.get
  Put s' -> S.put s'


newtype Error e r where
  Error :: e -> Error e r


throwError :: Member (Error e) r => e -> Eff r a
throwError = send . Error
{-# INLINE throwError #-}


runError :: Eff (Error e ': r) a -> Eff r (Either e a)
runError = shortCircuit $ \(Error e) -> E.throwE e


runErrorRelay :: Eff (Error e ': r) a -> Eff r (Either e a)
runErrorRelay = relay (pure . Right) $ \(Error e) _ -> pure $ Left e


subsume :: Member eff r => Eff (eff ': r) ~> Eff r
subsume = interpret send

