{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}

-- | Everything you need in order to define new effects.
module Polysemy.Effect.New
  ( Effect (..)
  , defaultHoist
  , makeSemantic
  , makeSemantic_
  , interpret
  , reinterpret
  , stateful
  , Union ()
  , decomp
  , prj
  , prjCoerce
  , weaken
  ) where

import           Control.Monad.Trans.State.Strict (StateT)
import qualified Control.Monad.Trans.State.Strict as S
import           Data.Tuple
import           Polysemy
import           Polysemy.Effect
import           Polysemy.Effect.TH
import           Polysemy.Union


------------------------------------------------------------------------------
-- | Interpret an effect by rewriting it in terms of capabilities already
-- present in the underlying effect stack.
interpret
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic r x)
    -> Semantic (e ': r) a
    -> Semantic r a
interpret f (Semantic m) = m $ \u ->
  case decomp u of
    Left  x -> liftSemantic $ hoist (interpret f) x
    Right y -> f y
{-# INLINE interpret #-}


------------------------------------------------------------------------------
-- | A highly-performant combinator for interpreting an effect statefully. See
-- 'stateful' for a more user-friendly variety of this function.
interpretInStateT
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT f s (Semantic m) = Semantic $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ()) (uncurry $ interpretInStateT' f)
            $ x
        Right y -> S.mapStateT (usingSemantic k) $ f y
{-# INLINE interpretInStateT #-}


------------------------------------------------------------------------------
-- | Loop breaker so that GHC will properly inline 'interpretInStateT'.
interpretInStateT'
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> StateT s (Semantic r) x)
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
interpretInStateT' = interpretInStateT
{-# NOINLINE interpretInStateT' #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but instead of removing the effect @e@, reencodes it in
-- some new effect @f@. This function will fuse when followed by
-- 'Polysemy.State.runState', meaning it's free to 'reinterpret' in terms of
-- the 'Polysemy.State.State' effect and immediately run it.
--
-- TODO(sandy): Make this fuse in with 'stateful' directly.
reinterpret
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> Semantic (f ': r) x)
    -> Semantic (e ': r) a
    -> Semantic (f ': r) a
reinterpret f (Semantic m) = Semantic $ \k -> m $ \u ->
  case prjCoerce u of
    Left x -> k $ hoist (reinterpret' f) $ x
    Right y  -> usingSemantic k $ f y
{-# INLINE[3] reinterpret #-}


------------------------------------------------------------------------------
-- | Lopo breaker so that GHC will properly inline 'reinterpret'.
reinterpret'
    :: Effect f
    => (∀ x. f (Semantic (f ': r)) x -> Semantic (g ': r) x)
    -> Semantic (f ': r) a
    -> Semantic (g ': r) a
reinterpret' = reinterpret
{-# NOINLINE reinterpret' #-}


------------------------------------------------------------------------------
-- | Like 'interpret', but with access to an intermediate state @s@.
stateful
    :: Effect e
    => (∀ x. e (Semantic (e ': r)) x -> s -> Semantic r (s, x))
    -> s
    -> Semantic (e ': r) a
    -> Semantic r (s, a)
stateful f = interpretInStateT $ \e -> S.StateT $ fmap swap . f e
{-# INLINE[3] stateful #-}

