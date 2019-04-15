{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Random
  ( -- * Effect
    Random (..)

    -- * Actions
  , random
  , randomR

    -- * Interpretations
  , runRandom
  , runRandomIO
  ) where

import           Polysemy
import           Polysemy.State
import qualified System.Random as R

------------------------------------------------------------------------------
-- | An effect capable of providing 'R.Random' values.
data Random m a where
  Random :: R.Random x => Random m x
  RandomR :: R.Random x => (x, x) -> Random m x

makeSem ''Random


------------------------------------------------------------------------------
-- | Run a 'Random' effect with an explicit 'R.RandomGen'.
runRandom
    :: forall q r a
     . ( Typeable q
       , R.RandomGen q
       )
    => q
    -> Sem (Random ': r) a
    -> Sem r (q, a)
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


------------------------------------------------------------------------------
-- | Run a 'Random' effect by using the 'IO' random generator.
runRandomIO :: Member (Lift IO) r => Sem (Random ': r) a -> Sem r a
runRandomIO m = do
  q <- sendM R.newStdGen
  snd <$> runRandom q m
{-# INLINE runRandomIO #-}

