{-# LANGUAGE RecursiveDo #-}
module FinalSpec where

import Test.Hspec

import qualified Control.Concurrent.Async as A
import Control.Concurrent.MVar
import Control.Concurrent.STM.TVar

import Data.Either
import Data.IORef

import Polysemy
import Polysemy.Final


import Polysemy.Async
import Polysemy.Error
import Polysemy.Fixpoint
import Polysemy.Trace
import Polysemy.State

import Polysemy.Writer

data Node a = Node a (IORef (Node a))

mkNode :: (Member (Embed IO) r, Member Fixpoint r)
       => a
       -> Sem r (Node a)
mkNode a = mdo
  let nd = Node a p
  p <- embed $ newIORef nd
  return nd

linkNode :: Member (Embed IO) r
         => Node a
         -> Node a
         -> Sem r ()
linkNode (Node _ r) b =
  embed $ writeIORef r b

readNode :: Node a -> a
readNode (Node a _) = a

follow :: Member (Embed IO) r
       => Node a
       -> Sem r (Node a)
follow (Node _ ref) = embed $ readIORef ref

test1 :: IO (Either Int (String, Int, Maybe Int))
test1 = do
  ref <- newIORef "abra"
  runFinal
    . embedToFinal @IO
    . runStateIORef ref -- Order of these interpreters don't matter
    . errorToIOFinal
    . fixpointToFinal @IO
    . asyncToIOFinal
     $ do
     n1 <- mkNode 1
     n2 <- mkNode 2
     linkNode n2 n1
     aw <- async $ do
       linkNode n1 n2
       modify (++"hadabra")
       n2' <- follow n2
       throw (readNode n2')
     m <- await aw `catch` (\s -> return $ Just s)
     n1' <- follow n1
     s <- get
     return (s, readNode n1', m)

test2 :: IO ([String], Either () ())
test2 =
    runFinal
  . runTraceList
  . errorToIOFinal
  . asyncToIOFinal
  $ do
  fut <- async $ do
    trace "Global state semantics?"
  catch @() (trace "What's that?" *> throw ()) (\_ -> return ())
  _ <- await fut
  trace "Nothing at all."


test3 :: IO (String, String)
test3 = do
  tvar <- newTVarIO ""
  (listened, _) <- runFinal . asyncToIOFinal . runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ tell " received"
      await a
  end <- readTVarIO tvar
  return (end, listened)

test4 :: IO (String, String)
test4 = do
  tvar <- newTVarIO ""
  lock <- newEmptyMVar
  (listened, a) <- runFinal . asyncToIOFinal . runWriterTVar tvar $ do
    tell "message "
    listen $ do
      tell "has been"
      a <- async $ do
        embedFinal $ takeMVar lock
        tell " received"
      return a
  putMVar lock ()
  _ <- A.wait a
  end <- readTVarIO tvar
  return (end, listened)

test5 :: Sem '[Error (A.Async (Maybe ())), Final IO] String
test5 = do
  tvar <- embedFinal $ newTVarIO ""
  lock <- embedFinal $ newEmptyMVar
  let
      inner = do
        tell "message "
        fmap snd $ listen @String $ do
          tell "has been"
          a <- async $ do
            embedFinal $ takeMVar lock
            tell " received"
          throw a
  asyncToIOFinal (runWriterTVar tvar inner) `catch` \a ->
    embedFinal $ do
      putMVar lock ()
      (_ :: Maybe ()) <- A.wait a
      readTVarIO tvar

spec :: Spec
spec = do
  describe "Final on IO" $ do
    it "should terminate successfully, with no exceptions,\
        \ and have global state semantics on State." $ do
      res1 <- test1
      res1 `shouldSatisfy` isRight
      case res1 of
        Right (s, i, j) -> do
          i `shouldBe` 2
          j `shouldBe` Just 1
          s `shouldBe` "abrahadabra"
        _ -> pure ()

    it "should treat trace with local state semantics" $ do
      res2 <- test2
      res2 `shouldBe` (["Nothing at all."], Right ())

  describe "runWriterTVar" $ do
    it "should listen and commit asyncs spawned and awaited upon in a listen \
       \block" $ do
      (end, listened) <- test3
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been received"

    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block has finished." $ do
      (end, listened) <- test4
      end `shouldBe` "message has been received"
      listened `shouldBe` "has been"


    it "should commit writes of asyncs spawned inside a listen block even if \
       \the block failed for any reason." $ do
      Right end1 <- runFinal . errorToIOFinal $ test5
      Right end2 <- runFinal . runError $ test5
      end1 `shouldBe` "message has been received"
      end2 `shouldBe` "message has been received"
