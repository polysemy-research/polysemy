module InsertSpec where

import Polysemy.State (State)
import Polysemy.Reader (Reader)
import Polysemy.Internal (insertAt)
import Polysemy
import Test.Hspec

insertAtWithIndex :: Sem (e1 : e2 : e3 : e4 : r) a -> Sem (e1 : e2 : Reader i : e3 : e4 : r) a
insertAtWithIndex = insertAt @2

insertAtAndRaiseUnder :: Sem (e1 : r) a -> Sem (e1 : e2 : State s : e3 : Reader i : e4 : r) a
insertAtAndRaiseUnder = raise2Under . insertAt @2 . raiseUnder

insertAtEmpty :: Sem (e1 : e2 : r) a -> Sem (e1 : e2 : r) a
insertAtEmpty = insertAt @2

spec :: Spec
spec = parallel $ do
  describe "insert" $ do
    it "insert" $ do
      1 `shouldBe` 1
