module TacticsSpec where

import Polysemy
import Polysemy.Internal (send)
import Test.Hspec

data TestE :: Effect where
  TestE :: m a -> (a -> m b) -> TestE m b

interpretTestE :: InterpreterFor TestE r
interpretTestE =
  interpretH $ \case
    TestE ma f -> do
      a <- runTH ma
      bindTH f a

spec :: Spec
spec = parallel $ describe "runTH and bindTH" $ do
  it "should act as expected" $ do
    r <- runM (interpretTestE (send (TestE (pure 5) (pure . (9 +)))))
    print r
    (14 :: Int) `shouldBe` r
