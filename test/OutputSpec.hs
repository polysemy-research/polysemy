module OutputSpec where

import Polysemy
import Polysemy.Output
import Data.Foldable
import Test.Hspec


spec :: Spec
spec = do
  describe "runBatchOutput" $ do
    it "should return nothing at batch size 0" $ do
      let (ms, ()) = runOutput 0 $ output @[Int] [0..100]
      length ms `shouldBe` 0

    for_ (1 : [5..13] ++ [99..101]) $ \size -> do
      it ("should batch at size " ++ show size) $ do
          let (ms, ()) = runOutput size $ do
                output @[Int] [0]
                output @[Int] [1..4]
                output @[Int] [5..10]
                output @[Int] [11..12]
                output @[Int] [13..37]
                output @[Int] [38..99]
          length ms `shouldBe` (div 100 size + 1)
          length (head ms) `shouldBe` min size 100
          concat ms `shouldBe` [0..99]


runOutput :: Int -> Sem '[Output [Int]] a -> ([[Int]], a)
runOutput size = run . runFoldMapOutput id . runBatchOutput size

