module Main (main) where

import Control.Monad (replicateM_)

import qualified Control.Monad.Except as MTL
import qualified Control.Monad.State as MTL
import qualified Control.Monad.Free as Free

import Criterion (bench, bgroup, whnf)
import Criterion.Main (defaultMain)

import Control.Monad.Freer (Member, Eff, run, send)
import Control.Monad.Freer.Internal (Eff(..), decomp, qApp, tsingleton)
import Control.Monad.Freer.Error (runError, throwError)
import Control.Monad.Freer.State (get, put, runState)

import qualified Control.Eff as EE
import qualified Control.Eff.Exception as EE
import qualified Control.Eff.State.Lazy as EE

--------------------------------------------------------------------------------
                        -- State Benchmarks --
--------------------------------------------------------------------------------

oneGet :: Int -> (Int, Int)
oneGet n = run (runState n get)

oneGetMTL :: Int -> (Int, Int)
oneGetMTL = MTL.runState MTL.get

oneGetEE :: Int -> (Int, Int)
oneGetEE n = EE.run $ EE.runState n EE.get

countDown :: Int -> (Int, Int)
countDown start = run (runState start go)
  where go = get >>= (\n -> if n <= 0 then pure n else put (n-1) >> go)

countDownMTL :: Int -> (Int, Int)
countDownMTL = MTL.runState go
  where go = MTL.get >>= (\n -> if n <= 0 then pure n else MTL.put (n-1) >> go)

countDownEE :: Int -> (Int, Int)
countDownEE start = EE.run $ EE.runState start go
  where go = EE.get >>= (\n -> if n <= 0 then pure n else EE.put (n-1) >> go)

--------------------------------------------------------------------------------
                       -- Exception + State --
--------------------------------------------------------------------------------
countDownExc :: Int -> Either String (Int,Int)
countDownExc start = run $ runError (runState start go)
  where go = get >>= (\n -> if n <= (0 :: Int) then throwError "wat" else put (n-1) >> go)

countDownExcMTL :: Int -> Either String (Int,Int)
countDownExcMTL = MTL.runStateT go
  where go = MTL.get >>= (\n -> if n <= (0 :: Int) then MTL.throwError "wat" else MTL.put (n-1) >> go)

countDownExcEE :: Int -> Either String (Int,Int)
countDownExcEE start = EE.run $ EE.runError (EE.runState start go)
  where go = EE.get >>= (\n -> if n <= (0 :: Int) then EE.throwError "wat" else EE.put (n-1) >> go)

--------------------------------------------------------------------------------
                          -- Freer: Interpreter --
--------------------------------------------------------------------------------
data Http out where
  Open :: String -> Http ()
  Close :: Http ()
  Post  :: String -> Http String
  Get   :: Http String

open' :: Member Http r => String -> Eff r ()
open'  = send . Open

close' :: Member Http r => Eff r ()
close' = send Close

post' :: Member Http r => String -> Eff r String
post' = send . Post

get' :: Member Http r => Eff r String
get' = send Get

runHttp :: Eff (Http ': r) w -> Eff r w
runHttp (Val x) = pure x
runHttp (E u q) = case decomp u of
  Right (Open _) -> runHttp (qApp q ())
  Right Close    -> runHttp (qApp q ())
  Right (Post d) -> runHttp (qApp q d)
  Right Get      -> runHttp (qApp q "")
  Left u'        -> E u' (tsingleton (runHttp . qApp q ))

--------------------------------------------------------------------------------
                          -- Free: Interpreter --
--------------------------------------------------------------------------------
data FHttpT x
  = FOpen String x
  | FClose x
  | FPost String (String -> x)
  | FGet (String -> x)
    deriving Functor

type FHttp = Free.Free FHttpT

fopen' :: String -> FHttp ()
fopen' s = Free.liftF $ FOpen s ()

fclose' :: FHttp ()
fclose' = Free.liftF $ FClose ()

fpost' :: String -> FHttp String
fpost' s = Free.liftF $ FPost s id

fget' :: FHttp String
fget' = Free.liftF $ FGet id

runFHttp :: FHttp a -> Maybe a
runFHttp (Free.Pure x) = pure x
runFHttp (Free.Free (FOpen _ n)) = runFHttp n
runFHttp (Free.Free (FClose n))  = runFHttp n
runFHttp (Free.Free (FPost s n)) = pure s  >>= runFHttp . n
runFHttp (Free.Free (FGet n))    = pure "" >>= runFHttp . n

--------------------------------------------------------------------------------
                        -- Benchmark Suite --
--------------------------------------------------------------------------------
prog :: Member Http r => Eff r ()
prog = open' "cats" >> get' >> post' "cats" >> close'

prog' :: FHttp ()
prog' = fopen' "cats" >> fget' >> fpost' "cats" >> fclose'

p :: Member Http r => Int -> Eff r ()
p count   =  open' "cats" >> replicateM_ count (get' >> post' "cats") >>  close'

p' :: Int -> FHttp ()
p' count  = fopen' "cats" >> replicateM_ count (fget' >> fpost' "cats") >> fclose'

main :: IO ()
main =
  defaultMain [
    bgroup "State" [
        bench "freer.get"          $ whnf oneGet 0
      , bench "mtl.get"            $ whnf oneGetMTL 0
      , bench "ee.get"             $ whnf oneGetEE 0
    ],
    bgroup "Countdown Bench" [
        bench "freer.State"    $ whnf countDown 10000
      , bench "mtl.State"      $ whnf countDownMTL 10000
      , bench "ee.State"       $ whnf countDownEE 10000
    ],
    bgroup "Countdown+Except Bench" [
        bench "freer.ExcState"  $ whnf countDownExc 10000
      , bench "mtl.ExceptState" $ whnf countDownExcMTL 10000
      , bench "ee.ExcState"     $ whnf countDownExcEE 10000
    ],
    bgroup "HTTP Simple DSL" [
        bench "freer" $ whnf (run . runHttp) prog
      , bench "free" $ whnf runFHttp prog'

      , bench "freerN"      $ whnf (run . runHttp . p) 1000
      , bench "freeN"       $ whnf (runFHttp . p')     1000
    ]
  ]
