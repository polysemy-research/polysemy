{-# LANGUAGE TemplateHaskell             #-}
{-# OPTIONS_GHC -fdefer-type-errors      #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module AmbiguousSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity
import Data.Monoid
import Polysemy
import Polysemy.Embed (runEmbedded)
import Polysemy.State
import Test.Hspec
import Test.ShouldNotTypecheck


uniquelyInt :: Members '[State Int , State String] r => Sem r ()
uniquelyInt = put 10

uniquelyString :: Members '[State Int , State String] r => Sem r ()
uniquelyString = put mempty

uniquelyIO :: Members '[Embed IO, Embed Identity] r => Sem r ()
uniquelyIO = embed $ liftIO $ pure ()

ambiguous1 :: Members '[State (Sum Int), State String] r => Sem r ()
ambiguous1 = put mempty

ambiguous2 :: (Num String, Members '[State Int, State String] r) => Sem r ()
ambiguous2 = put 10


spec :: Spec
spec = describe "example" $ do
  it "should run uniquelyInt" $ do
    let z = run . runState 0 . runState "hello" $ uniquelyInt
    z `shouldBe` (10, ("hello", ()))

  it "should run uniquelyString" $ do
    let z = run . runState 0 . runState "hello" $ uniquelyString
    z `shouldBe` (0, ("", ()))

  it "should run uniquelyIO" $ do
    z <- runM . runEmbedded @Identity (pure . runIdentity) $ uniquelyIO
    z `shouldBe` ()

  it "should not typecheck ambiguous1" $ do
    shouldNotTypecheck ambiguous1

  it "should not typecheck ambiguous2" $ do
    shouldNotTypecheck ambiguous2

