{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Reader where

import Polysemy
import Polysemy.Effect.New

data Reader i m a
  = Ask (i -> a)
  | ∀ x. Local (i -> i) (m x) (x -> a)

deriving instance Functor (Reader i m)

instance Effect (Reader i) where
  weave s _ (Ask k) = Ask $ fmap (<$ s) k
  weave s distrib (Local f m k) =
    Local f (distrib $ m <$ s) (fmap k)
  {-# INLINE weave #-}

  hoist _ (Ask k)       = Ask k
  hoist h (Local f m k) = Local f (h m) k
  {-# INLINE hoist #-}

makeSemantic ''Reader


runReader :: i -> Semantic (Reader i ': r) a -> Semantic r a
runReader i = interpret $ \case
  Ask k -> pure $ k i
  Local f m k -> fmap k $ runReader' (f i) m
{-# INLINE runReader #-}


runReader' :: i -> Semantic (Reader i ': r) a -> Semantic r a
runReader' = runReader
{-# NOINLINE runReader' #-}

