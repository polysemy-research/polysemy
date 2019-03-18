{-# LANGUAGE BlockArguments   #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

import Test.Inspection
import Control.Monad.Discount
import Data.OpenUnion
import TRYAGAIN hiding (main)
import qualified Control.Monad.Trans.State.Strict as S
import qualified Control.Monad.Trans.Except as E


main :: IO ()
main = pure ()

go :: Eff '[State Int] Int
go = do
  n <- send (Get id)
  if n <= 0
     then pure n
     else do
       send $ Put (n-1) ()
       go

tryIt :: Either Bool String
tryIt = run . runError @Bool $ do
  catch @Bool
    do
      throw False
    \_ -> pure "hello"

countDown :: Int -> Int
countDown start = fst $ run $ runState start go

inspect $ 'countDown `hasNoType`  ''SNat
inspect $ 'countDown `doesNotUse` ''S.StateT
inspect $ 'tryIt     `doesNotUse` ''E.ExceptT

