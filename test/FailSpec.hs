module FailSpec where

import Polysemy
import Polysemy.Fail
import Polysemy.NonDet
import Test.Hspec
import Control.Applicative

semFail :: Member Fail r => Maybe Bool -> Sem r Bool
semFail mb = do
  Just b <- pure mb
  pure b

runAlt :: Alternative f => Sem '[Fail, NonDet] a -> f a
runAlt = run . runNonDet . failToNonDet

spec :: Spec
spec = parallel $ do
  describe "MonadFail instance with failToNonDet" $ do
    it "should call empty via fail" $ do
      runAlt (semFail Nothing) `shouldBe` Nothing
      runAlt (semFail Nothing) `shouldBe` []
    it "should work fine for non-failing patterns" $ do
      runAlt (semFail $ Just True) `shouldBe` Just True
      runAlt (semFail $ Just False) `shouldBe` [False]
