{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

import Test.Inspection
import Control.Monad.Discount
import Data.OpenUnion
import TRYAGAIN hiding (main)


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

countDown :: Int -> Int
countDown start = fst $ run $ runState start go

inspect $ 'countDown `hasNoType` ''SNat

