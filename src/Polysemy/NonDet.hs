{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.NonDet
  ( -- * Effect
    NonDet (..)

    -- * Interpretations
  , runNonDet
  ) where

import Control.Applicative
import Polysemy.Internal
import Polysemy.Internal.NonDet
import Polysemy.Internal.Union
import Polysemy.Internal.Effect


--------------------------------------------------------------------------------
-- This stuff is lifted from 'fused-effects'. Thanks guys!
runNonDetC :: (Alternative f, Applicative m) => NonDetC m a -> m (f a)
runNonDetC (NonDetC m) = m (fmap . (<|>) . pure) (pure empty)
{-# INLINE runNonDetC #-}


newtype NonDetC m a = NonDetC
  { -- | A higher-order function receiving two parameters: a function to combine
    -- each solution with the rest of the solutions, and an action to run when no
    -- results are produced.
    unNonDetC :: forall b . (a -> m b -> m b) -> m b -> m b
  }
  deriving (Functor)

instance Applicative (NonDetC m) where
  pure a = NonDetC (\ cons -> cons a)
  {-# INLINE pure #-}

  NonDetC f <*> NonDetC a = NonDetC $ \ cons ->
    f (\ f' -> a (cons . f'))
  {-# INLINE (<*>) #-}

instance Alternative (NonDetC m) where
  empty = NonDetC (\ _ nil -> nil)
  {-# INLINE empty #-}

  NonDetC l <|> NonDetC r = NonDetC $ \ cons -> l cons . r cons
  {-# INLINE (<|>) #-}

instance Monad (NonDetC m) where
  NonDetC a >>= f = NonDetC $ \ cons ->
    a (\ a' -> unNonDetC (f a') cons)
  {-# INLINE (>>=) #-}


------------------------------------------------------------------------------
-- | Run a 'NonDet' effect in terms of some underlying 'Alternative' @f@.
runNonDet :: Alternative f => Sem (NonDet ': r) a -> Sem r (f a)
runNonDet (Sem m) = Sem $ \k -> runNonDetC $ m $ \u ->
  case decomp u of
    Left x  -> NonDetC $ \cons nil -> do
      z <- k $ weave [()] (fmap concat . traverse runNonDet) x
      foldr cons nil z
    Right (Yo Empty _ _ _) -> empty
    Right (Yo (Choose ek) s _ y) -> do
      z <- pure (ek True) <|> pure (ek False)
      pure $ y $ z <$ s

