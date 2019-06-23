{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies    #-}

module Polysemy.Dispatch where

import Control.Monad
import Control.Concurrent.MVar
import Control.Concurrent.Chan.Unagi
import Polysemy
import Polysemy.Internal
import Polysemy.Internal.Union


data Request r end = forall a. Request
  { responseMVar :: MVar (Sem end a)
  , request      :: Union r (Sem r) a
  }

dispatchEverything
    :: ( LastMembers end r
       , Member (Lift IO) end
       )
    => InChan (Request r end)
    -> Sem r a
    -> Sem end a
dispatchEverything chan (Sem m) = Sem $ \k -> m $ \u -> do
  case decompLast u of
    Left x -> usingSem k $ join $ sendM $ do
      mvar <- newEmptyMVar
      writeChan chan $ Request mvar x
      takeMVar mvar
    Right y -> k $ hoist (dispatchEverything chan) y


receiveEverything
    :: ( LastMembers end r
       , Member (Lift IO) r
       )
    => OutChan (Request r end)
    -> Sem r a
receiveEverything chan = Sem $ \k -> forever $ do
  Request mvar req <- k $ inj $ Lift $ readChan chan
  resp <- k req
  k $ inj $ Lift $ putMVar mvar $ pure resp

