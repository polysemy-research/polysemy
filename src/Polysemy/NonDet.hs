{-# LANGUAGE DeriveAnyClass  #-}
{-# LANGUAGE TemplateHaskell #-}

module Polysemy.NonDet
  ( -- * Effect
    NonDet (..)

    -- * Interpretations
  , runNonDet
  , runNonDetMaybe
  , nonDetToError
  ) where

import Control.Applicative
import Control.Monad.Trans.Maybe

import Polysemy
import Polysemy.Error
import Polysemy.Internal
import Polysemy.Internal.NonDet
import Polysemy.Internal.Union

------------------------------------------------------------------------------
-- | Run a 'NonDet' effect in terms of some underlying 'Alternative' @f@.
runNonDet :: Alternative f => Sem (NonDet ': r) a -> Sem r (f a)
runNonDet = runNonDetC . runNonDetInC
{-# INLINE runNonDet #-}

------------------------------------------------------------------------------
-- | Run a 'NonDet' effect in terms of an underlying 'Maybe'
--
-- Unlike 'runNonDet', uses of '<|>' will not execute the
-- second branch at all if the first option succeeds.
runNonDetMaybe :: Sem (NonDet ': r) a -> Sem r (Maybe a)
runNonDetMaybe (Sem sem) = Sem $ \k -> runMaybeT $ sem $ \u ->
  case decomp u of
    Right (Weaving e s wv ex _) ->
      case e of
        Empty -> empty
        Choose left right ->
          MaybeT $ usingSem k $ runMaybeT $ fmap ex $ do
              MaybeT (runNonDetMaybe (wv (left <$ s)))
          <|> MaybeT (runNonDetMaybe (wv (right <$ s)))
    Left x -> MaybeT $
      k $ weave (Just ())
          (maybe (pure Nothing) runNonDetMaybe)
          id
          x
{-# INLINE runNonDetMaybe #-}

------------------------------------------------------------------------------
-- | Transform a 'NonDet' effect into an @'Error' e@ effect,
-- through providing an exception that 'empty' may be mapped to.
--
-- This allows '<|>' to handle 'throw's of the @'Error' e@ effect.
nonDetToError :: Member (Error e) r
              => e
              -> Sem (NonDet ': r) a
              -> Sem r a
nonDetToError (e :: e) = interpretH $ \case
  Empty -> throw e
  Choose left right -> do
    left'  <- nonDetToError e <$> runT left
    right' <- nonDetToError e <$> runT right
    raise (left' `catch` \(_ :: e) -> right')
{-# INLINE nonDetToError #-}


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

runNonDetInC :: Sem (NonDet ': r) a -> NonDetC (Sem r) a
runNonDetInC = usingSem $ \u ->
  case decomp u of
    Left x  -> consC $ fmap getNonDetState $
      liftSem $ weave (NonDetState (Just ((), empty)))
                  distribNonDetC
                  -- TODO(KingoftheHomeless): Is THIS the right semantics?
                  (fmap fst . getNonDetState)
                  x
    Right (Weaving Empty _ _ _ _) -> empty
    Right (Weaving (Choose left right) s wv ex _) -> fmap ex $
      runNonDetInC (wv (left <$ s)) <|> runNonDetInC (wv (right <$ s))
{-# INLINE runNonDetInC #-}

-- This choice of functorial state is inspired from the
-- MonadBaseControl instance for 'ListT' from 'list-t'.
--
-- TODO(KingoftheHomeless):
-- Is there a different representation of this which doesn't require
-- 'unconsC' in 'distribNonDetC'?
newtype NonDetState r a = NonDetState {
  getNonDetState :: Maybe (a, NonDetC (Sem r) a)
  } deriving (Functor)

-- KingoftheHomeless: The performance of this is terrible
-- since unconsC is O(n), but this really only matters if
--
--   1. You have higher-order effects interpreted after 'NonDet'
--
--   2. You use '<|>' a /lot/ inside higher-order actions of those effects.
--
-- You can fix this by instead creating a NonDet carrier based upon
-- reflection without remorse, but that would require a lot of work, and will
-- slightly degrade performance when this ISN'T a problem.
-- Question is if it's worth it.
distribNonDetC :: NonDetState r (Sem (NonDet ': r) a) -> Sem r (NonDetState r a)
distribNonDetC = \case
  NonDetState (Just (a, r)) ->
    fmap NonDetState $ unconsC $ runNonDetInC a <|> (r >>= runNonDetInC)
  _ ->
    pure (NonDetState Nothing)
{-# INLINE distribNonDetC #-}

-- O(n)
unconsC :: NonDetC (Sem r) a -> Sem r (Maybe (a, NonDetC (Sem r) a))
unconsC (NonDetC n) = n (\a r -> pure (Just (a, consC r))) (pure Nothing)
{-# INLINE unconsC #-}

consC :: Sem r (Maybe (a, NonDetC (Sem r) a)) -> NonDetC (Sem r) a
consC m = NonDetC $ \cons nil -> m >>= \case
  Just (a, r) -> cons a (unNonDetC r cons nil)
  _           -> nil
{-# INLINE consC #-}
