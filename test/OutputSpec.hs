module OutputSpec where

import Polysemy
import Polysemy.Output
import Data.Foldable
import Test.Hspec


spec :: Spec
spec = parallel $ do
  describe "runBatchOutput" $ do
    it "should return nothing at batch size 0" $ do
      let (ms, _) = runOutput 0 $ traverse (output @Int) [0..99]
      length ms `shouldBe` 0

    for_ (1 : [5..13] ++ [99..101]) $ \size ->
      context ("Works at size " ++ show size) $ do
        let (ms, _) = runOutput size $ traverse (output @Int) [0..99]
        it "returns the correct amount of batches" $
          length ms `shouldBe` div (100 + size - 1) size -- 100 `div` size but rounding up
        it "all batches except the last one are of the specified size" $
          map length (init ms) `shouldBe` replicate (length ms - 1) size
        it "returns all original elements in the correct order" $
          concat ms `shouldBe` [0..99]

  describe "runOutputAsList" $
    it "should return elements in the order they were output" $
      let (xs, ()) = runOutputAsList' $ traverse_ (output @Int) [0..100]
       in xs `shouldBe` [0..100]


runOutput :: Int -> Sem '[Output Int] a -> ([[Int]], a)
runOutput size = run . runFoldMapOutput (:[]) . runBatchOutput size

runOutputAsList' :: Sem '[Output Int] a -> ([Int], a)
runOutputAsList' = run . runOutputAsList
