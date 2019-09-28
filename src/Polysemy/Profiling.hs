{-# LANGUAGE AllowAmbiguousTypes, BangPatterns, DeriveTraversable, TemplateHaskell #-}
module Polysemy.Profiling where

import Control.Monad
import Control.Concurrent.Async
import Control.Exception

import Data.IORef
import Data.Word
import GHC.Clock
import System.Mem

import Polysemy
import Polysemy.Final
import Polysemy.Output
import Polysemy.Reader

data ProfilingResult a
  = TimeProfile Double
  | MemProfile Int
  | OtherProfile a
  deriving (Show, Eq, Ord, Read, Functor, Foldable, Traversable)

data Profiling m a where
  Profile       :: String -> m a -> Profiling m a

makeSem ''Profiling




infixr 5 `thenProfile`
thenProfile :: (forall x. Sem r x -> Sem r (ProfilingResult s, x))
            -> (forall x. Sem r x -> Sem r ([ProfilingResult s], x))
            -> Sem r a -> Sem r ([ProfilingResult s], a)
thenProfile a b = mergeResult . a . b

mergeResult :: Sem r (a, ([a], b))
            -> Sem r ([a], b)
mergeResult = fmap (\(x, (xs, a)) -> (x:xs, a))

profileTime :: forall s r a
             . Member (Embed IO) r
            => Sem r a
            -> Sem r (ProfilingResult s, a)
profileTime sem = do
  timeBefore <- embed getMonotonicTime
  !a         <- sem
  timeAfter  <- embed getMonotonicTime
  let !timeTaken = timeAfter - timeBefore
  return (TimeProfile timeTaken, a)

newtype Profiler s r =
  Profiler { getProfiler :: forall x. Sem r x -> Sem r (ProfilingResult s, x) }

runProfiling' :: forall s r a
              . Member (Output (String, [ProfilingResult s])) r
             => [Profiler s r]
             -> Sem (Profiling ': r) a
             -> Sem r a
runProfiling' profilers = interpretH $ \case
  Profile str m -> do
    m' <- runProfiling profileAction <$> runT m
    (report, a) <- raise $ profileAction $ m' >>= (\ !a -> return a)
    output (str, report)
    return a
  where
    profileAction :: forall x. Sem r x -> Sem r ([ProfilingResult s], x)
    profileAction sem =
      foldr
        (\(Profiler c) b -> mergeResult (c b))
        (endProfiling sem)
        profilers

profileMem :: forall s r a
            . Member (Embed IO) r
           => Sem r a
           -> Sem r (ProfilingResult s, a)
profileMem sem = do
  allocBefore <- embed getAllocationCounter
  !a          <- sem
  allocAfter  <- embed getAllocationCounter
  let !allocation = fromIntegral $ negate (allocAfter - allocBefore)
  return (MemProfile allocation, a)

endProfiling :: forall s r a
              . Sem r a
             -> Sem r ([ProfilingResult s], a)
endProfiling = ((\ !r -> ([], r)) <$!>)

runProfiling :: forall s r a
              . Member (Output (String, [ProfilingResult s])) r
             => (forall x. Sem r x -> Sem r ([ProfilingResult s], x))
             -> Sem (Profiling ': r) a
             -> Sem r a
runProfiling profileAction = interpretH $ \case
  Profile str m -> do
    m' <- runProfiling profileAction <$> runT m
    (report, a) <- raise $ profileAction $ m' >>= (\ !a -> return a)
    output (str, report)
    return a

ignoreProfiling :: Sem (Profiling ': r) a
                -> Sem r a
ignoreProfiling = interpretH $ \case
  Profile _ m -> do
    m' <- ignoreProfiling <$> runT m
    raise m'

profilingToIOTime
  :: Member (Embed IO) r
  => Sem (Profiling ': r) a
  -> Sem r ([(String, Double)], a)
profilingToIOTime sem = do
  ref <- embed $ newIORef []
  let
    go :: Member (Embed IO) r
       => Sem (Profiling ': r) a
       -> Sem r a
    go = interpretH $ \case
      Profile str m -> do
        m' <- go <$> runT m
        timeBefore <- embed getMonotonicTime
        !fa <- raise m'
        timeAfter <- embed getMonotonicTime
        let !timeTaken = timeAfter - timeBefore
        embed $ atomicModifyIORef' ref (\l -> ((str, timeTaken) : l, ()))
        return fa
  a   <- go sem
  end <- embed $ readIORef ref
  return (reverse end, a)

profilingToOutputTime
  :: (Member (Embed IO) r, Member (Output (String, Double)) r)
  => Sem (Profiling ': r) a
  -> Sem r a
profilingToOutputTime = interpretH $ \case
  Profile str m -> do
    m' <- profilingToOutputTime <$> runT m
    timeBefore <- embed getMonotonicTime
    !fa <- raise m'
    timeAfter <- embed getMonotonicTime
    let !timeTaken = timeAfter - timeBefore
    output $ (str, timeTaken)
    return fa

profilingToOutputTimeMem
  :: (Member (Embed IO) r, Member (Output (String, Double, Int)) r)
  => Sem (Profiling ': r) a
  -> Sem r a
profilingToOutputTimeMem = interpretH $ \case
  Profile str m -> do
    m' <- profilingToOutputTimeMem <$> runT m
    allocBefore <- embed getAllocationCounter
    timeBefore  <- embed getMonotonicTime
    !fa  <- raise m'
    timeAfter <- embed getMonotonicTime
    allocAfter <- embed getAllocationCounter
    let !timeTaken = timeAfter - timeBefore
    let !memAlloced = fromIntegral @_ @Int (negate (allocAfter - allocBefore))
    output $ (str, timeTaken, memAlloced)
    return fa

seqEither :: Either e a -> ()
seqEither (Right !_) = ()
seqEither (Left !_) = ()

profilingToOutputAccurateTimeMem
  :: (Member (Final IO) r, Member (Output (String, Double, Int)) r)
  => Sem (Profiling ': r) a
  -> Sem r a
profilingToOutputAccurateTimeMem = interpretH $ \case
  Profile str m -> do
    m' <- profilingToOutputAccurateTimeMem <$> runT m
    withWeavingToFinal $ \s wv _ -> do
      let !m'' = wv (raise m' <$ s)
      mask $ \restore -> do
        allocBefore <- getAllocationCounter
        timeBefore  <- getMonotonicTime
        res <- try @SomeException (restore m'')
        pure $! seqEither res
        timeAfter <- getMonotonicTime
        allocAfter <- getAllocationCounter
        let !timeTaken = timeAfter - timeBefore
        let !memAlloced = fromIntegral @_ @Int (negate (allocAfter - allocBefore)) -- getAllocationCounter decreases with more allocaiton
        case res of
          Right fa -> wv ((\a -> output (str, timeTaken, memAlloced) >> return a) <$> fa)
          Left  e  -> wv (output (str, timeTaken, memAlloced) <$ s) >> throwIO e

profileToOutput
  :: (Member (Final IO) r, Member (Output (String, Double)) r)
  => Sem (Profiling ': r) a
  -> Sem r a
profileToOutput = interpretH $ \case
  Profile str m -> do
    m' <- profileToOutput <$> runT m
    raise $ withWeavingToFinal $ \s wv _ -> do
      let !m'' = wv (m' <$ s)
      timeBefore <- getMonotonicTime
      !res <- m''
      timeAfter <- getMonotonicTime
      wv ((\a -> output (str, timeAfter - timeBefore) >> return a)  <$> res)


addTimeProfiling :: Members '[
                    Profiling
                  , Embed IO
                  , Output (String, Double)
                  ] r
                 => Sem r a
                 -> Sem r a
addTimeProfiling = interceptH $ \(Profile str m) -> do
    !m' <- subsume <$> runT m
    timeBefore <- embed getMonotonicTime
    res <- raise $ profile str (m' >>= \ !fa -> return fa)
    timeAfter <- embed getMonotonicTime
    let !timeTaken = timeAfter - timeBefore
    output (str, timeTaken)
    return res

exa :: (Member Profiling r, Member (Reader Bool) r) => Sem r ()
exa = local (\_ -> True) $ profile "woop" $ wasteTime (1000000 :: Integer)
  where
    wasteTime 0 = pure ()
    wasteTime !i = wasteTime (i - 1)

profilingToOutputTimeSem
  :: forall s r a
   . (Member (Output (String, [ProfilingResult s])) r, Member (Embed IO) r)
  => Sem (Profiling ': r) a
  -> Sem r a
profilingToOutputTimeSem = runProfiling @s (profileTime `thenProfile` profileMem `thenProfile` endProfiling)
