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

  it "should be strict in the output" $
    let
      t1 = runM . runWriter @String $ do
        tell @String (error "strict")

      t2 = runM . runWriter @String $ do
        listen @String (tell @String (error "strict"))

      t3 = runM . runWriter @String $
        pass @String $ pure (\_ -> error "strict", ())
    in do
      t1 `shouldThrow` errorCall "strict"
      t2 `shouldThrow` errorCall "strict"
      t3 `shouldThrow` errorCall "strict"
