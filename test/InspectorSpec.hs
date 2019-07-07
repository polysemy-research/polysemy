{-# LANGUAGE TemplateHaskell #-}

module InspectorSpec where

import Control.Monad
import Data.IORef
import Polysemy
import Polysemy.Error
import Polysemy.State
import Test.Hspec



data Callback m a where
  Callback :: m String -> Callback m ()

makeSem ''Callback



spec :: Spec
spec = parallel $ describe "Inspector" $ do
  it "should inspect State effects" $ do
    withNewTTY $ \ref -> do
      void . (runM .@ runCallback ref)
           . runState False
           $ do
        embed $ pretendPrint ref "hello world"
        callback $ show <$> get @Bool
        modify not
        callback $ show <$> get @Bool

      result <- readIORef ref
      result `shouldContain` ["hello world"]
      result `shouldContain` ["False", "True"]

  it "should not inspect thrown Error effects" $ do
    withNewTTY $ \ref -> do
      void . (runM .@ runCallback ref)
           . runError @()
           $ do
        callback $ throw ()
        callback $ pure "nice"

      result <- readIORef ref
      result `shouldContain` [":(", "nice"]


runCallback
    :: Member (Embed IO) r
    => IORef [String]
    -> (forall x. Sem r x -> IO x)
    -> Sem (Callback ': r) a
    -> Sem r a
runCallback ref lower = interpretH $ \case
  Callback cb -> do
    cb' <- runT cb
    ins <- getInspectorT
    embed $ doCB ref $ do
      v <- lower .@ runCallback ref $ cb'
      pure $ maybe ":(" id $ inspect ins v
    getInitialStateT


doCB :: IORef [String] -> IO String -> IO ()
doCB ref m = m >>= pretendPrint ref


pretendPrint :: IORef [String] -> String -> IO ()
pretendPrint ref msg = modifyIORef ref (++ [msg])


withNewTTY :: (IORef [String] -> IO a) -> IO a
withNewTTY f = do
  ref <- newIORef []
  f ref

