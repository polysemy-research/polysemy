{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module ExampleSpec where

import Polysemy
import Polysemy.Error
import Polysemy.Input
import Polysemy.Output
import Polysemy.Resource
import Test.Hspec

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Teletype, Resource, Error CustomException] r => Sem r ()
program = catch @CustomException work $ \e -> writeTTY ("Caught " ++ show e)
  where work = bracket (readTTY) (const $ writeTTY "exiting bracket") $ \i -> do
          writeTTY "entering bracket"
          case i of
            "explode"     -> throw ThisException
            "weird stuff" -> writeTTY i >> throw ThatException
            _             -> writeTTY i >> writeTTY "no exceptions"

foo :: IO (Either CustomException ())
foo =
    runFinal
  . embedToFinal @IO
  . resourceToIOFinal
  . errorToIOFinal @CustomException
  . teletypeToIO
  $ program

spec :: Spec
spec = describe "example" $ do
  it "should compile!" $ do
    True `shouldBe` True

