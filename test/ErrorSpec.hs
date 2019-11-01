module ErrorSpec where

import qualified Control.Exception as X
import           Polysemy
import           Polysemy.Error
import           Polysemy.Resource
import           Test.Hspec

newtype MyExc = MyExc String
  deriving (Show, Eq)

instance X.Exception MyExc

spec :: Spec
spec = parallel $ do
  describe "fromException" $ do
    it "should catch exceptions" $ do
      a <-
        runM $ runError $ fromException @MyExc $ do
          _ <- X.throwIO $ MyExc "hello"
          pure ()
      a `shouldBe` (Left $ MyExc "hello")

    it "should not catch non-exceptions" $ do
      a <-
        runM $ runError @MyExc $ fromException @MyExc $ pure ()
      a `shouldBe` Right ()

    it "should happen before Resource" $ do
      a <-
        runM $ resourceToIO $ runError @MyExc $ do
          onException
            (fromException @MyExc $ do
              _ <- X.throwIO $ MyExc "hello"
              pure ()
            ) $ pure $ error "this exception shouldn't happen"
      a `shouldBe` (Left $ MyExc "hello")

