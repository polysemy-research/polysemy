{-# LANGUAGE AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses #-}
module Polysemy.TestMeta where

import Data.Function
import Polysemy
import Polysemy.HigherOrder
import Polysemy.Final
import Polysemy.Trace
import Polysemy.Async
import Polysemy.Writer
import Control.Concurrent (threadDelay)
import Polysemy.State
import Polysemy.Bracket hiding (Bracket(..), generalBracket, bracketToIOFinal)
import Polysemy.Meta
import qualified Control.Exception as X


type Bracket = Meta Bracket'

data Bracket' :: MetaEffect where
  GeneralBracket
    :: m a
    -> (a -> ExitCase b -> z c)
    -> (a -> m b)
    -> Bracket' '[z :% UninterruptibleMask] m (b, c)

generalBracket
    :: Member Bracket r
    => Sem r a
    -> (a -> ExitCase b -> Sem (UninterruptibleMask ': r) c)
    -> (a -> Sem r b)
    -> Sem r (b, c)
generalBracket alloc dealloc use =
  sendMeta $ GeneralBracket alloc dealloc use

type UninterruptibleMask = Meta UninterruptibleMask'

data UninterruptibleMask' :: MetaEffect where
  Interruptible :: m a -> UninterruptibleMask' '[] m a
  UninterruptibleMask :: z a -> UninterruptibleMask' '[z :% MaskRestore] m a

interruptible :: Member UninterruptibleMask r => Sem r a -> Sem r a
interruptible = sendMeta . Interruptible

uninterruptibleMask :: Member UninterruptibleMask r => Sem (MaskRestore ': r) a -> Sem r a
uninterruptibleMask = sendMeta . UninterruptibleMask

data MaskRestore :: Effect where
  MaskRestore :: m a -> MaskRestore m a

maskRestore :: Member MaskRestore r => Sem r a -> Sem r a
maskRestore = send . MaskRestore

uninterruptibleMaskToFinalIO :: Member (Final IO) r
                             => InterpreterFor UninterruptibleMask r
uninterruptibleMaskToFinalIO = interpretMeta $ \e -> controlFinal @IO $ \lwr ->
  case e of
    Interruptible m -> X.interruptible (lwr (runH m))
    UninterruptibleMask m -> X.uninterruptibleMask $ \restore ->
      runMeta m
      & interpretH \case
          MaskRestore m' -> controlFinal $ \lwr' -> restore $ lwr' $ runH m'
      & lwr

bracketToIOFinal :: Member (Final IO) r
                 => InterpreterFor Bracket r
bracketToIOFinal = interpretMeta \case
  GeneralBracket alloc dealloc use -> withLoweringToFinal $ controlL $ \lwr ->
    X.mask \restore -> lwr $ do
      let release a ec = withProcessorL $ \lwr' ->
            runMeta (dealloc a ec)
            & uninterruptibleMaskToFinalIO
            & lwr'
            & X.try @X.SomeException
      a <- runL (runH alloc)
      etb <- withProcessorL $ \lower' -> X.try $ restore (lower' (runH (use a)))
      case etb of
        Left e -> do
          _ <- release a (ExitCaseException e)
          embed (X.throwIO e)
        Right tb | Just tVoid <- traverse (const Nothing) tb -> do
          _ <- release a ExitCaseAbort
          restoreL tVoid
        Right tb -> do
          b <- restoreL tb
          c <-     release a (ExitCaseSuccess b)
               >>= either (embed . X.throwIO) return
               >>= restoreL
          return (b, c)



testProgram :: IO ()
testProgram = runM $ traceToStderr $ bracketToIOFinal $ asyncToIOFinal $ do
  a <- async $ do
    generalBracket
      (trace "acquire")
      (\_ _ -> uninterruptibleMask $ do
          trace "begun cleanup"
          embed $ threadDelay 2000000
          trace "done cleanup"
          maskRestore (interruptible (return ()))
          trace "should not happen"
      )
      (\_ -> trace "using resource")
  embed $ threadDelay 1000000
  trace "cancelling..."
  cancel a
  trace "cancelled"

data Constic (e :: Effect) :: MetaEffect where
  Constic :: e m a -> Constic e '[] m a

runWriteric :: Monoid o => Sem (Meta (Constic (Writer o)) ': r) a -> Sem r (o, a)
runWriteric =
    runState mempty
  . interpretMeta \case
      Constic (Tell o) -> modify' (<> o)
      Constic (Listen m) -> do
        (o, ta) <- runWriteric (runExposeH' m)
        modify' (<> o)
        a <- restoreH ta
        return (o, a)
      Constic (Pass m) -> do
        (o, tfa) <- runWriteric (runExposeH' m)
        let f = foldr (const . fst) id tfa
        modify' (<> f o)
        snd <$> restoreH tfa
  . raiseUnder


-- type Exceptional exc eff = Meta (ExceptionalParam exc eff)

-- data ExceptionalParam exc eff :: (EffectRow -> Effect -> Effect) where
--   ExceptionalParam :: forall exc eff m a. m a -> ExceptionalParam exc eff '[Fatal exc] eff m a

-- exceptional :: forall exc eff r a. Members '[Exceptional exc eff, Fatal exc] r => Sem (eff ': r) a -> Sem r a
-- exceptional m = subsume $ toMeta (ExceptionalParam @exc @eff (raiseUnder m))

-- data Write :: Effect where
--   Write :: String -> Write m ()

-- data WriteParam :: EffectRow -> Effect -> Effect where
--   OpenFile :: FilePath -> m a -> WriteParam '[] Write m a

-- type FileIO = Exceptional IOError (Meta WriteParam)

-- fileIOToFinal :: Member (Final IO) r
--               => InterpreterFor FileIO r
-- fileIOToFinal = runMeta $ \n (ExceptionalParam m) ->
--   n m
--   & raiseUnder
--   & runMeta \n' (OpenFile fp m') ->
--       fatalFromExceptionSem @IOError $ controlFinal $ \lwr ->
--         withFile fp WriteMode $ \h ->
--           n' m'
--           & interpret \case
--               Write str -> embedFinal $ hPutStrLn h str
--           & lwr

-- data IdWriter :: MetaEffect where
--   IdWriter :: Monoid o => m a -> IdWriter '[Writer o] (Writer o) n m a

-- data IdState :: MetaEffect where
--   IdState :: forall s m a n. m a -> IdState '[State s] (State s) n m a

-- data IdError :: MetaEffect where
--   IdError :: forall e m a n. m a -> IdError '[Error e] (Error e) n m a


-- runIdWriter :: InterpreterFor (Meta IdWriter) r
-- runIdWriter = runMeta $ \_ n (IdWriter m) -> n m

-- runIdState :: InterpreterFor (Meta IdState) r
-- runIdState = runMeta $ \_ n (IdState m) -> n m

-- runIdError :: InterpreterFor (Meta IdError) r
-- runIdError = runMeta $ \_ n (IdError m) -> n m

-- tester :: (String, String)
-- tester = run $ runIdWriter $ runWriter $ subsume $ toMeta $ IdWriter @String $ do
--   (s, _) <- listen $ do
--     tell "wow"
--     raise $ tell "woah"
--   tell "madre"
--   raise $ tell "dios"
--   return s


-- tester2 :: (String, String)
-- tester2 = run $ runIdState $ runState "" $ do
--   put "one"
--   a <- subsume $ toMeta $ IdState @String $ do
--     raise (modify (++"two"))
--     modify (++"three")
--     get
--   return a
