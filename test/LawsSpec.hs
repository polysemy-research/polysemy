module LawsSpec where

import Polysemy.Law
import Polysemy.State
import Polysemy.State.Law
import Test.Hspec

spec :: Spec
spec = parallel $ do
  describe "State effects" $ do
    it "runState should pass the laws" $
      property $ runStateLaws $ fmap snd . runState @Int 0
    it "runLazyState should pass the laws" $
      property $ runStateLaws $ fmap snd . runLazyState @Int 0
    it "stateToIO should pass the laws" $
      property $ runStateLawsIO $ fmap snd . stateToIO @Int 0
