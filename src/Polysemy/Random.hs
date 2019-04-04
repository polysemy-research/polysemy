{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random where

import           Polysemy
import           Polysemy.Effect.New
import           Polysemy.State
import qualified System.Random as R

data Random m a
  = ∀ x. R.Random x => Random (x -> a)
  | ∀ x. R.Random x => RandomR (x, x) (x -> a)

deriving instance Functor (Random m)
deriving instance Effect Random

makeSemantic ''Random


runRandom
    :: forall q r a
     . R.RandomGen q
    => q
    -> Semantic (Random ': r) a
    -> Semantic r (q, a)
runRandom q = runState q . reinterpret \case
  Random k -> do
    ~(a, q') <- gets @q R.random
    put q'
    pure $ k a
  RandomR r k -> do
    ~(a, q') <- gets @q $ R.randomR r
    put q'
    pure $ k a
{-# INLINE runRandom #-}


runRandomIO :: Member (Lift IO) r => Semantic (Random ': r) a -> Semantic r a
runRandomIO m = do
  q <- sendM R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}

