{-# LANGUAGE BlockArguments  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Output
  ( -- * Effect
    Output (..)

    -- * Actions
  , output

    -- * Interpretations
  , runFoldMapOutput
  , runIgnoringOutput
  , runBatchOutput
  ) where

import Polysemy
import Polysemy.State


------------------------------------------------------------------------------
-- | An effect capable of sending messages. Useful for streaming output and for
-- logging.
data Output o m a where
  Output :: o -> Output o m ()

makeSem ''Output


------------------------------------------------------------------------------
-- | Run an 'Output' effect by transforming it into a monoid.
runFoldMapOutput
    :: forall o m r a
     . (Typeable m, Monoid m)
    => (o -> m)
    -> Sem (Output o ': r) a
    -> Sem r (m, a)
runFoldMapOutput f = runState mempty . reinterpret \case
  Output o -> modify (<> f o)
{-# INLINE runFoldMapOutput #-}


------------------------------------------------------------------------------
-- | Run an 'Ouput' effect by ignoring it.
runIgnoringOutput :: Sem (Output o ': r) a -> Sem r a
runIgnoringOutput = interpret \case
  Output _ -> pure ()
{-# INLINE runIgnoringOutput #-}


------------------------------------------------------------------------------
-- | Accumulate 'output's so they are delayed until they reach at least size
-- @size@.
--
-- If @size@ is 0, this interpretation will not emit anything in the resulting
-- 'Output' effect.
--
-- @since 0.1.2.0
runBatchOutput
    :: forall o r a
     . (Typeable o)
    => Int
    -> Sem (Output [o] ': r) a
    -> Sem (Output [[o]] ': r) a
runBatchOutput 0 m = raise $ runIgnoringOutput m
runBatchOutput size m = do
  ((_, res), a) <-
    runState (0 :: Int, [] :: [o]) $ reinterpret2 (\case
      Output o -> do
        (nacc, acc) <- get
        let no     = length o
            total  = acc <> o
            ntotal = nacc + no

            emitting n ls
              | n >= size = do
                  let (emit, acc') = splitAt size ls
                  output [emit]
                  emitting (n - size) acc'
              | otherwise = pure (n, ls)
        (nacc', acc') <- emitting ntotal total
        put (nacc', acc')
    ) m
  output [res]
  pure a

