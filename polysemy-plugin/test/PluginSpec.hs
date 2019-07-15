{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}

module PluginSpec where

import Data.Functor.Identity
import GHC.Exts
import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Output
import Test.Hspec
import Unsafe.Coerce



idState :: Member (State s) r => Sem r ()
idState = do
  s <- get
  put s

intState :: Member (State Int) r => Sem r ()
intState = put 10

numState :: Num a => Member (State a) r => Sem r ()
numState = put 10

strState :: Member (State String) r => Sem r ()
strState = put "hello"

oStrState :: IsString a => Member (State a) r => Sem r ()
oStrState = put "hello"


err :: Member (Error e) r => Sem r Bool
err =
  catch
    (throw undefined)
    (\_ -> pure True)


errState :: Num s => Members '[Error e, State s] r => Sem r Bool
errState = do
  numState
  err


lifted :: Monad m => Member (Embed m) r => Sem r ()
lifted = embed $ pure ()


newtype MyString = MyString String
  deriving (IsString, Eq, Show)


data Janky = forall s. Janky (forall i. Sem '[State s] ())

jankyState :: Janky
jankyState = Janky $ put True

unsafeUnjank :: Janky -> Sem '[State Bool] ()
unsafeUnjank (Janky sem) = unsafeCoerce sem


spec :: Spec
spec = do
  describe "State effect" $ do
    describe "get/put" $ do
      it "should work in simple cases" $ do
        flipShouldBe (True, ()) . run $ runState True idState

      it "should, when polymorphic, eliminate the first matching effect" $ do
        flipShouldBe (False, (True, ()))   . run $ runState False $ runState True idState

      it "should, when polymorphic, not eliminate unmatching effects" $ do
        flipShouldBe (True, Right @Int ()) . run $ runState True $ runError idState

    describe "numbers" $ do
      it "should interpret against concrete Int" $ do
        flipShouldBe (10, ()) . run $ runState 0 intState

      describe "polymorphic Num constraint" $ do
        it "should interpret against Int" $ do
          flipShouldBe (10 :: Int, ())     . run $ runState 0 numState

        it "should interpret against Float" $ do
          flipShouldBe (10 :: Float, ())   . run $ runState 0 numState

        it "should interpret against Double" $ do
          flipShouldBe (10 :: Double, ())  . run $ runState 0 numState

        it "should interpret against Integer" $ do
          flipShouldBe (10 :: Integer, ()) . run $ runState 0 numState

    describe "strings" $ do
      it "concrete interpret against concrete String" $ do
        flipShouldBe ("hello", ()) . run $ runState "nothing" strState

      describe "polymorphic IsString constraint" $ do
        it "should interpret against String" $ do
          flipShouldBe ("hello" :: String, ())   . run $ runState "nothing" oStrState

        it "should interpret against MyString" $ do
          flipShouldBe ("hello" :: MyString, ()) . run $ runState "nothing" oStrState

    describe "existential state" $ do
      it "JankyState should compile" $ do
        flipShouldBe (True, ()) . run $ runState False $ unsafeUnjank jankyState


  describe "Error effect" $ do
    it "should interpret against Int" $ do
      flipShouldBe (Right @Int True)  . run $ runError err
    it "should interpret against Bool" $ do
      flipShouldBe (Right @Bool True) . run $ runError err


  describe "State/Error effect" $ do
    it "should interpret against Int/String" $ do
      flipShouldBe (10 :: Int, Right @String True)  . run $ runState 0 $ runError errState
    it "should interpret against Float/Bool" $ do
      flipShouldBe (10 :: Float, Right @Bool True)  . run $ runState 0 $ runError errState


  describe "Error/State effect" $ do
    it "should interpret against String/Int" $ do
      flipShouldBe (Right @String (10 :: Int, True))  . run $ runError $ runState 0 errState
    it "should interpret against Bool/Float" $ do
      flipShouldBe (Right @Bool (10 :: Float, True))  . run $ runError $ runState 0 errState

  describe "Output effect" $ do
    it "should unify recursively with tyvars" $ do
      flipShouldBe 11 . sum . fst . run . runOutputMonoid id $ do
        output [1]
        output $ replicate 2 5


  describe "Embed effect" $ do
    it "should interpret against IO" $ do
      res <- runM lifted
      res `shouldBe` ()

    it "should interpret against Identity" $ do
      let res = runM lifted
      res `shouldBe` Identity ()


flipShouldBe :: (Show a, Eq a) => a -> a -> Expectation
flipShouldBe = flip shouldBe

