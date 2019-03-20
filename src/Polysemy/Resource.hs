{-# LANGUAGE UnicodeSyntax #-}

module Polysemy.Resource
  ( Resource (..)
  , bracket
  , runResource
  ) where

import qualified Control.Exception as X
import           Control.Monad (void)
import           Polysemy
import           Polysemy.Effect.New


data Resource m a
  = ∀ r x. Bracket (m r) (r -> m ()) (r -> m x) (x -> a)

deriving instance Functor (Resource m)

instance Effect Resource where
  weave s f (Bracket alloc dealloc use k) =
    Bracket (f $ alloc <$ s)
            (void . f . fmap dealloc)
            (f . fmap use)
            (fmap k)
  {-# INLINE weave #-}

  hoist f (Bracket alloc dealloc use k) =
    Bracket (f alloc) (fmap f dealloc) (fmap f use) k
  {-# INLINE hoist #-}


bracket
    :: Member Resource r
    => Semantic r a
    -> (a -> Semantic r ())
    -> (a -> Semantic r b)
    -> Semantic r b
bracket alloc dealloc use = send $ Bracket alloc dealloc use id
{-# INLINE bracket #-}


runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource finish = interpret $ \case
  Bracket alloc dealloc use k -> fmap k . sendM $
    let runIt :: Semantic (Resource ': r) x -> IO x
        runIt = finish . runResource' finish
     in X.bracket
          (runIt alloc)
          (runIt . dealloc)
          (runIt . use)
{-# INLINE runResource #-}


runResource'
    :: Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource' = runResource
{-# NOINLINE runResource' #-}

