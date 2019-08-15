{-# LANGUAGE AllowAmbiguousTypes #-}
module WriterSpec where

import Test.Hspec

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.Concurrent.STM
import Control.Exception (evaluate)

import Data.IORef

import Polysemy
import Polysemy.Async
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
spec = do
  describe "writer" $ do
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
  describe "runWriterIORef" $ do
    it "should listen and commit asyncs spawned within the block" $ do
      (end, listened) <- test4
      end `shouldBe` "abrahadabra"
      listened `shouldBe` "hadabra"
    it "should lose writes of asyncs spawned inside a listen block once the block \
       \has finished." $ do
      (end, listened) <- test5
      end `shouldBe` "abrahad"
      listened `shouldBe` "had"
  describe "runWriterTVar" $ do
    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block has finished." $ do
      (end, listened) <- test6
      end `shouldBe` "abrahadabra"
      listened `shouldBe` "had"


test4 :: IO (String, String)
test4 = do
  ref <- newIORef ""
  (listened, _) <- (runM .@ lowerAsync) . runWriterIORef ref $ do
    tell "abra"
    listen $ do
      tell "had"
      a <- async $ tell "abra"
      await a
  end <- readIORef ref
  return (end, listened)

test5 :: IO (String, String)
test5 = do
  ref  <- newIORef ""
  lock <- newEmptyMVar
  (listened, a) <- (runM .@ lowerAsync) . runWriterIORef ref $ do
    tell "abra"
    listen $ do
      tell "had"
      a <- async $ do
        embed $ takeMVar lock
        tell "abra"
      return a
  putMVar lock ()
  _ <- A.wait a
  end <- readIORef ref
  return (end, listened)

test6 :: IO (String, String)
test6 = do
  ref  <- newTVarIO ""
  lock <- newEmptyMVar
  (listened, a) <- (runM .@ lowerAsync) . runWriterTVar ref $ do
    tell "abra"
    listen $ do
      tell "had"
      a <- async $ do
        embed $ takeMVar lock
        tell "abra"
      return a
  putMVar lock ()
  _   <- A.wait a
  end <- readTVarIO ref
  return (end, listened)
