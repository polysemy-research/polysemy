{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fdefer-type-errors            #-}
{-# OPTIONS_GHC -fno-warn-deferred-type-errors #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin       #-}

module AmbiguousSpec where

import Control.Monad.IO.Class (liftIO)
import Data.Functor.Identity
import Data.Monoid
import Polysemy
import Polysemy.Embed (runEmbedded)
import Polysemy.State
import Test.Hspec
import Test.ShouldNotTypecheck

class MPTC a b where
  mptc :: a -> b

instance MPTC Bool Int where
  mptc _ = 1000


uniquelyInt :: Members '[State Int , State String] r => Sem r ()
uniquelyInt = put 10

uniquelyA :: forall a b r. (Num a, Members '[State a, State b] r) => Sem r ()
uniquelyA = put 10

uniquelyString :: Members '[State Int , State String] r => Sem r ()
uniquelyString = put mempty

uniquelyB :: (MPTC Bool b, Members '[State String, State b] r) => Sem r ()
uniquelyB = put $ mptc False

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

  it "should run uniquelyA" $ do
    let z = run . runState 0 . runState "hello" $ uniquelyA @Int @String
    z `shouldBe` (10, ("hello", ()))

  it "should run uniquelyB" $ do
    let z = run . runState 0 . runState "hello" $ uniquelyB @Int
    z `shouldBe` (1000, ("hello", ()))

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

