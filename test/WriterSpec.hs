{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import Polysemy
import Polysemy.Error
import Polysemy.Writer

censor' :: forall e s a r
        . (Member (Error e) r, Member (Writer s) r)
        => (s -> s)
        -> Sem r a
        -> Sem r a
censor' f m = do
  res <- censor f $ fmap Right m `catch` (pure . Left)
  case res of
      Right res' -> return res'
      Left e -> throw (e :: e)

test1 :: (String, Either () ())
test1 =
    run
  . runWriter
  . runError $
  do
    tell "censoring"
    censor @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test2 :: (String, Either () ())
test2 =
    run
  . runWriter
  . runError $
  do
    tell "censoring"
    censor' @() @String
      (drop 4)
      (tell " not applied" *> throw ())
    `catch`
      (\(_ :: ()) -> pure ())

test3 :: (String, (String, ()))
test3 = run . runWriter $ listen (tell "and hear")

spec :: Spec
spec = describe "writer" $ do
  it "should not censor" $ do
    test1 `shouldBe` ("censoring not applied", Right ())

  it "should censor" $ do
    test2 `shouldBe` ("censoring applied", Right ())

  it "should have a proper listen" $ do
    test3 `shouldBe` ("and hear", ("and hear", ()))
