module AlternativeSpec where

import Polysemy
import Polysemy.NonDet
import Test.Hspec
import Control.Applicative
import Polysemy.Trace
import Polysemy.Reader

runAlt :: Alternative f => Sem '[NonDet] a -> f a
runAlt = run . runNonDet

failtrace :: (Member NonDet r, Member Trace r)
          => Sem r ()
failtrace = pure () <|> trace "trace"

failtrace' :: (Member NonDet r, Member Trace r)
           => Sem r ()
failtrace' = trace "sim" *> empty <|> trace "salabim"

inHigherOrder :: Members '[NonDet, Trace, Reader ()] r
              => Sem r ()
inHigherOrder = do
  local (\_ -> ()) $ trace "1" <|> trace "2"
  trace "3"

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

  describe "runNonDet" $ do
    it "should have terrible semantics when <|> is used within\
       \ a higher-order action of a later effect (See Issue #246.)" $ do
      (fst . run . runTraceList . runReader () . runNonDet @[]) inHigherOrder
        `shouldBe` ["1","2","3","3"]
      (fst . run . runTraceList . runReader () . runNonDet @[]) inHigherOrder
        `shouldNotBe` ["1","3","2","3"] -- This is what we actually WANT it to be.
      (fst . run . runTraceList . runNonDet @[] . runReader ()) inHigherOrder
        `shouldBe` ["1","3","2","3"]
