module OutputSpec where

import Control.Concurrent.STM
import Control.Exception (evaluate)

import Data.IORef
import Data.Foldable

import Polysemy
import Polysemy.Async
import Polysemy.Output
import Polysemy.Final

import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "runOutputBatched" $ do
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

  describe "runOutputList" $
    it "should return elements in the order they were output" $
      let (xs, ()) = runOutputList' $ traverse_ (output @Int) [0..100]
       in xs `shouldBe` [0..100]

  describe "runOutputMonoid" $
    it "should be strict in the output" $
      let t = runOutputMonoid (id @String) $ do
            output @String (error "strict")
            return ()
      in do
        runM t           `shouldThrow` errorCall "strict"
        evaluate (run t) `shouldThrow` errorCall "strict"

  describe "runOutputMonoidIORef" $ do
    it "should commit writes of asynced computations" $
      let io = do
            ref <- newIORef ""
            runFinal
              . embedToFinal @IO
              . asyncToIOFinal
              . runOutputMonoidIORef ref (show @Int)
              $ test1
            readIORef ref
      in do
        res <- io
        res `shouldBe` "12"

  describe "runOutputMonoidTVar" $ do
    it "should commit writes of asynced computations" $
      let io = do
            ref <- newTVarIO ""
            runFinal
              . embedToFinal @IO
              . asyncToIOFinal
              . runOutputMonoidTVar ref (show @Int)
              $ test1
            readTVarIO ref
      in do
        res <- io
        res `shouldBe` "12"

runOutput :: Int -> Sem '[Output Int, Output [Int]] a -> ([[Int]], a)
runOutput size = run . runOutputMonoid (:[]) . runOutputBatched size

runOutputList' :: Sem '[Output Int] a -> ([Int], a)
runOutputList' = run . runOutputList

test1 :: Members '[Async, Output Int] r
     => Sem r ()
test1 = do
  output @Int 1
  a <- async $ do
    output @Int 2
  _ <- await a
  return ()
