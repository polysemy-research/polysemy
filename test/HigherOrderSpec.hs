module HigherOrderSpec where

import Polysemy
import Polysemy.Reader
import Test.Hspec


spec :: Spec
spec = parallel $ describe "Reader local" $ do
  it "should nest with itself" $ do
    let foo = run . runReader "hello" $ do
                local (++ " world") $ do
                  local (++ "!") $ do
                    ask
    foo `shouldBe` "hello world!"

