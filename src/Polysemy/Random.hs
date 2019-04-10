{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random where

import           Polysemy
import           Polysemy.State
import qualified System.Random as R

data Random m a where
  Random :: R.Random x => Random m x
  RandomR :: R.Random x => (x, x) -> Random m x

makeSemantic ''Random


runRandom
    :: forall q r a
     . ( Typeable q
       , R.RandomGen q
       )
    => q
    -> Semantic (Random ': r) a
    -> Semantic r (q, a)
runRandom q = runState q . reinterpret \case
  Random -> do
    ~(a, q') <- gets @q R.random
    put q'
    pure a
  RandomR r -> do
    ~(a, q') <- gets @q $ R.randomR r
    put q'
    pure a
{-# INLINE runRandom #-}


runRandomIO :: Member (Lift IO) r => Semantic (Random ': r) a -> Semantic r a
runRandomIO m = do
  q <- sendM R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}

