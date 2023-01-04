{-# LANGUAGE AllowAmbiguousTypes #-}
module Polysemy.TestMeta where

import Data.Function
import Polysemy
import Polysemy.Final
import Polysemy.Writer
import Polysemy.Error
import Polysemy.State
import Polysemy.Scoped
import Polysemy.Fatal
import System.IO


type Exceptional exc eff = Meta (ExceptionalParam exc eff)

data ExceptionalParam exc eff :: (EffectRow -> Effect -> Effect) where
  ExceptionalParam :: forall exc eff m a. m a -> ExceptionalParam exc eff '[Fatal exc] eff m a

exceptional :: forall exc eff r a. Members '[Exceptional exc eff, Fatal exc] r => Sem (eff ': r) a -> Sem r a
exceptional m = subsume $ toMeta (ExceptionalParam @exc @eff (raiseUnder m))

data Write :: Effect where
  Write :: String -> Write m ()

data WriteParam :: EffectRow -> Effect -> Effect where
  OpenFile :: FilePath -> m a -> WriteParam '[] Write m a

type FileIO = Exceptional IOError (Meta WriteParam)

fileIOToFinal :: Member (Final IO) r
              => InterpreterFor FileIO r
fileIOToFinal = runMeta $ \n (ExceptionalParam m) ->
  n m
  & raiseUnder
  & runMeta \n' (OpenFile fp m') ->
      fatalFromExceptionSem @IOError $ controlFinal $ \lwr ->
        withFile fp WriteMode $ \h ->
          n' m'
          & interpret \case
              Write str -> embedFinal $ hPutStrLn h str
          & lwr

data IdWriter :: (EffectRow -> Effect -> Effect) where
  IdWriter :: Monoid o => m a -> IdWriter '[Writer o] (Writer o) m a

data IdState :: (EffectRow -> Effect -> Effect) where
  IdState :: forall s m a. m a -> IdState '[State s] (State s) m a

data IdError :: (EffectRow -> Effect -> Effect) where
  IdError :: forall e m a. m a -> IdError '[Error e] (Error e) m a


runIdWriter :: InterpreterFor (Meta IdWriter) r
runIdWriter = runMeta $ \n (IdWriter m) -> n m

runIdState :: InterpreterFor (Meta IdState) r
runIdState = runMeta $ \n (IdState m) -> n m

runIdError :: InterpreterFor (Meta IdError) r
runIdError = runMeta $ \n (IdError m) -> n m
  -- n m
  -- & raiseUnder
  -- & interpretH \case
  --     Tell o   -> tell o
  --     Listen m -> listen (runH m)
  --     Pass m   -> pass (runH m)

tester :: (String, String)
tester = run $ runIdWriter $ runWriter $ toMeta $ IdWriter @String $ do
  (s, _) <- listen $ do
    tell "wow"
    raise $ tell "woah"
  tell "madre"
  raise $ tell "dios"
  return s


tester2 :: (String, String)
tester2 = run $ runIdState $ runState "" $ do
  put "one"
  a <- toMeta $ IdState @String $ do
    raise (modify (++"two"))
    modify (++"three")
    get
  return a
