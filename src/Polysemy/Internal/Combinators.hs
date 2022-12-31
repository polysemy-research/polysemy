{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# OPTIONS_HADDOCK not-home #-}

-- | Description: The basic interpreter-building combinators
module Polysemy.Internal.Combinators
  ( -- * First order
    rewrite
  , transform

    -- * Statefulness
  , stateful
  , lazilyStateful
  ) where

import           Control.Arrow ((>>>))
import           Control.Monad
import qualified Control.Monad.Trans.State.Lazy as LS
import qualified Control.Monad.Trans.State.Strict as S
import qualified Data.Tuple as S (swap)

import Polysemy.Internal
import Polysemy.Internal.Union

------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInStateT f s (Sem sem) = Sem $ \k ->
  (S.swap <$!>) $ flip S.runStateT s $ sem $ \u ->
    case decomp u of
        Left x ->
          liftHandlerWithNat
            (\m -> S.StateT $ \s' -> S.swap <$!> interpretInStateT f s' m)
            k x
        Right (Weaving e _ lwr ex) -> do
          let z = mkInitState lwr
          ex . (<$ z) <$> S.mapStateT (usingSem k) (f e)
{-# INLINE interpretInStateT #-}


------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInLazyStateT
    :: (∀ x m. e m x -> LS.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
interpretInLazyStateT f s (Sem sem) = Sem $ \k ->
  fmap S.swap $ flip LS.runStateT s $ sem $ \u ->
    case decomp u of
        Left x ->
          liftHandlerWithNat
            (\m -> LS.StateT $ \s' -> S.swap <$> interpretInLazyStateT f s' m)
            k x
        Right (Weaving e _ lwr ex) -> do
          let z = mkInitState lwr
          ex . (<$ z) <$> LS.mapStateT (usingSem k) (f e)
{-# INLINE interpretInLazyStateT #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ (S.swap <$!>) . f e
{-# INLINE[3] stateful #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
lazilyStateful
    :: (∀ x m. e m x -> s -> Sem r (s, x))
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
lazilyStateful f = interpretInLazyStateT $ \e -> LS.StateT $ fmap S.swap . f e
{-# INLINE[3] lazilyStateful #-}

------------------------------------------------------------------------------
-- | Rewrite an effect @e1@ directly into @e2@, and put it on the top of the
-- effect stack.
--
-- @'rewrite' n = 'interpretH' ('propagate' . n)@
--
-- @since 1.2.3.0
rewrite
    :: forall e1 e2 r a
     . (forall z x. e1 z x -> e2 z x)
    -> Sem (e1 ': r) a
    -> Sem (e2 ': r) a
rewrite f (Sem m) = Sem $ \k -> m $ \u ->
  k $ hoist (rewrite f) $ case decompCoerce u of
    Left x -> x
    Right (Weaving e mkT lwr ex) ->
      Union Here $ Weaving (f e) mkT lwr ex


------------------------------------------------------------------------------
-- | Transform an effect @e1@ into an effect @e2@ that is already somewhere
-- inside of the stack.
--
-- @since 1.2.3.0
transform
    :: forall e1 e2 r a
     . Member e2 r
    => (forall z x. e1 z x -> e2 z x)
    -> Sem (e1 ': r) a
    -> Sem r a
transform f (Sem m) = Sem $ \k -> m $ \u ->
  k $ hoist (transform f) $ case decomp u of
    Left g -> g
    Right (Weaving e mkT lwr ex) ->
      injWeaving (Weaving (f e) mkT lwr ex)
