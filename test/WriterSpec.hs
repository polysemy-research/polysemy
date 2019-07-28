{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import Control.Exception (evaluate)

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
      t1 = runWriter @String $ do
        tell @String (error "strict")
        return ()

      t2 = runWriter @String $ do
        _ <- listen @String (tell @String (error "strict"))
        return ()

      t3 = runWriter @String $ do
        pass @String $ pure (\_ -> error "strict", ())
        return ()
    in do
      runM t1           `shouldThrow` errorCall "strict"
      evaluate (run t1) `shouldThrow` errorCall "strict"
      runM t2           `shouldThrow` errorCall "strict"
      evaluate (run t2) `shouldThrow` errorCall "strict"
      runM t3           `shouldThrow` errorCall "strict"
      evaluate (run t3) `shouldThrow` errorCall "strict"
