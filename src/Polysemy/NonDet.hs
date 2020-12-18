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
import Control.Monad
import Control.Monad.Trans.Maybe
import Control.Monad.Trans

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
--
-- @since 1.1.0.0
runNonDetMaybe :: Sem (NonDet ': r) a -> Sem r (Maybe a)
runNonDetMaybe (Sem sem) = Sem $ \k -> runMaybeT $ sem $ \u ->
  case decomp u of
    Right (Weaving e mkT lwr ex) ->
      case e of
        Empty -> empty
        Choose left right ->
          MaybeT $ usingSem k $ runMaybeT $ fmap ex $
              MaybeT (runNonDetMaybe (lwr (mkT id left)))
          <|> MaybeT (runNonDetMaybe (lwr (mkT id right)))
    Left x -> liftHandlerWithNat (MaybeT . runNonDetMaybe) k x
{-# INLINE runNonDetMaybe #-}

------------------------------------------------------------------------------
-- | Transform a 'NonDet' effect into an @'Error' e@ effect,
-- through providing an exception that 'empty' may be mapped to.
--
-- This allows '<|>' to handle 'throw's of the @'Error' e@ effect.
--
-- @since 1.1.0.0
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

instance MonadTrans NonDetC where
  lift m = NonDetC $ \c b -> m >>= (`c` b)

instance MonadTransControl NonDetC where
  type StT NonDetC = []

  hoistT n nd = NonDetC $ \c b ->
    join $ n $ unNonDetC nd (\a r -> return $ c a (join (n r))) (return b)

  liftWith main = lift $ main (\m -> unNonDetC m (\a -> fmap (a:)) (return []))

  restoreT m = NonDetC $ \c b -> m >>= foldr c b

runNonDetInC :: Sem (NonDet ': r) a -> NonDetC (Sem r) a
runNonDetInC = usingSem $ \u ->
  case decomp u of
    Left x  -> liftHandlerWithNat runNonDetInC liftSem x
    Right (Weaving Empty _ _ _)-> empty
    Right (Weaving (Choose left right) mkT lwr ex) -> fmap ex $
      runNonDetInC (lwr (mkT id left)) <|> runNonDetInC (lwr (mkT id right))
{-# INLINE runNonDetInC #-}
