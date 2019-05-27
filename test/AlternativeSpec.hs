module AlternativeSpec where

import Polysemy
import Polysemy.NonDet
import Test.Hspec
import Control.Applicative

semFail :: Member NonDet r => Maybe Bool -> Sem r Bool
semFail mb = do
  Just b <- pure mb
  pure b


runAlt :: Alternative f => Sem '[NonDet] a -> f a
runAlt = run . runNonDet


spec :: Spec
spec = do
  describe "Alternative instance" $ do
    it "should choose the first branch" $ do
      runAlt (pure '1' <|> pure '2') `shouldBe` (Just '1')
    it "should failover" $ do
      runAlt (empty <|> pure '2') `shouldBe` (Just '2')
      runAlt (pure '1' <|> empty) `shouldBe` (Just '1')

  describe "MonadFail instance" $ do
    it "should call empty via fail" $ do
      runAlt (semFail Nothing) `shouldBe` Nothing
      runAlt (semFail Nothing) `shouldBe` []
    it "should work fine for non-failing patterns" $ do
      runAlt (semFail $ Just True) `shouldBe` Just True
      runAlt (semFail $ Just False) `shouldBe` [False]

