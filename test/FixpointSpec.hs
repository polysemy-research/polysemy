{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecursiveDo #-}
module FixpointSpec where

import Control.Exception (try, evaluate)
import Control.Monad.Fix

import Polysemy
import Polysemy.Fixpoint
import Polysemy.Error
import Polysemy.State
import Polysemy.Output

import Test.Hspec

data FinalState s m a where
  GetEventualState :: FinalState s m s

makeSem ''FinalState

runFinalState :: Member Fixpoint r
              => s
              -> Sem (State s ': FinalState s ': r) a
              -> Sem r (s, a)
runFinalState s sm = mfix $ \ ~(s', _) ->
  interpret
    (\GetEventualState -> pure s')
    (runState s sm)

test1 :: (String, (Int, ()))
test1 =
    run
  . runFixpoint run
  . runOutputMonoid (show @Int)
  . runFinalState 1
  $ do
  s  <- get @Int
  s' <- getEventualState @Int
  output @Int s
  output @Int s'
  put @Int 2

test2 :: Either [Int] [Int]
test2 =
    run
  . runFixpoint run
  . runError
  $ mdo
  a <- throw (1 : a) `catch` (\e -> return e)
  return a

test3 :: Either () (Int, Int)
test3 =
    run
  . runFixpoint run
  . runError
  . runLazyState @Int 1
  $ mdo
  put a
  a <- throw ()
  return a

test4 :: (Int, Either () Int)
test4 =
    run
  . runFixpoint run
  . runLazyState @Int 1
  . runError
  $ mdo
  put a
  a <- throw ()
  return a


spec :: Spec
spec = parallel $ describe "runFixpoint" $ do
  it "should work with runState" $ do
    test1 `shouldBe` ("12",  (2, ()))
  it "should work with runError" $ do
    let res = fmap (take 10) test2
    res `shouldBe` Right (replicate 10 1)
  it "should not trigger the bomb" $ do
    test3 `shouldBe` Left ()
  it "should trigger the bomb" $ do
    let (s, a) = test4
    evaluate s `shouldThrow` errorCall bombMessage
    a `shouldBe` Left ()

bombMessage :: String
bombMessage =
  "runFixpoint: Internal computation failed.\
              \ This is likely because you have tried to recursively use\
              \ the result of a failed computation in an action\
              \ whose effect may be observed even though the computation failed.\
              \ It's also possible that you're using an interpreter\
              \ that uses 'weave' improperly.\
              \ See documentation for more information."
