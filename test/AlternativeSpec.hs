module AlternativeSpec where

import Polysemy
import Polysemy.NonDet
import Test.Hspec
import Control.Applicative
import Polysemy.Trace

runAlt :: Alternative f => Sem '[NonDet] a -> f a
runAlt = run . runNonDet

failtrace :: (Member NonDet r, Member Trace r)
          => Sem r ()
failtrace = pure () <|> trace "trace"

failtrace' :: (Member NonDet r, Member Trace r)
           => Sem r ()
failtrace' = trace "sim" *> empty <|> trace "salabim"

spec :: Spec
spec = parallel $ do
  describe "Alternative instance" $ do
    it "should choose the first branch" $ do
      runAlt (pure '1' <|> pure '2') `shouldBe` (Just '1')
    it "should failover" $ do
      runAlt (empty <|> pure '2') `shouldBe` (Just '2')
      runAlt (pure '1' <|> empty) `shouldBe` (Just '1')

  describe "runNonDetMaybe" $ do
    it "should skip second branch if the first branch succeeds" $ do
      (run . runNonDetMaybe . runTraceList) failtrace
        `shouldBe` Just ([], ())
      (run . runTraceList . runNonDetMaybe) failtrace
        `shouldBe` ([], Just ())

    it "should respect local/global state semantics" $ do
      (run . runNonDetMaybe . runTraceList) failtrace'
        `shouldBe` Just (["salabim"], ())
      (run . runTraceList . runNonDetMaybe) failtrace'
        `shouldBe` (["sim", "salabim"], Just ())
