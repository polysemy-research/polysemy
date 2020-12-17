module TacticsSpec where

import Polysemy
import Polysemy.Internal (send)
import Polysemy.Internal.Tactics (seqEitherT, seqMaybeT)
import Test.Hspec

data TestE :: Effect where
  TestE :: m a -> (a -> m b) -> TestE m b

interpretTestE :: InterpreterFor TestE r
interpretTestE =
  interpretH $ \case
    TestE ma f -> do
      a <- runTSimple ma
      bindTSimple f a

data TestSeq :: Effect where
   SeqRight :: m a -> TestSeq m (Either String a)
   SeqLeft :: m a -> TestSeq m (Either String a)
   SeqJust :: m a -> TestSeq m (Maybe a)
   SeqNothing :: m a -> TestSeq m (Maybe a)

success :: Sem r a -> Sem r (Either String a)
success =
  fmap Right

failure :: Sem r a -> Sem r (Either String a)
failure =
  (Left "failed" <$)

interpretTestSeq :: InterpreterFor TestSeq r
interpretTestSeq =
  interpretH \case
    SeqRight ma ->
      seqEitherT =<< success (runTSimple ma)
    SeqLeft ma ->
      seqEitherT =<< failure (runTSimple ma)
    SeqJust ma ->
      seqMaybeT =<< fmap Just (runTSimple ma)
    SeqNothing ma ->
      seqMaybeT =<< (Nothing <$) (runTSimple ma)

spec :: Spec
spec = parallel $ do
  describe "runTSimple and bindTSimple" $ do
    it "should act as expected" $ do
      r <- runM (interpretTestE (send (TestE (pure 5) (pure . (9 +)))))
      (14 :: Int) `shouldBe` r
  describe "seqEitherT" $ do
    it "should sequence a Right" $ do
      r <- runM (interpretTestSeq (send (SeqRight (pure 5))))
      (Right (5 :: Int)) `shouldBe` r
    it "should sequence a Left" $ do
      r <- runM (interpretTestSeq (send (SeqLeft (pure 5))))
      (Left "failed") `shouldBe` r
  describe "seqMaybeT" $ do
    it "should sequence a Just" $ do
      r <- runM (interpretTestSeq (send (SeqJust (pure 5))))
      (Just (5 :: Int)) `shouldBe` r
    it "should sequence a Nothing" $ do
      r <- runM (interpretTestSeq (send (SeqNothing (pure 5))))
      Nothing `shouldBe` r
