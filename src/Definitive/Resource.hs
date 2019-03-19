{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE UnicodeSyntax       #-}

module Definitive.Resource
  ( Resource (..)
  , bracket
  , runResource
  ) where

import qualified Control.Exception as X
import           Control.Monad (void)
import           Definitive
import           Definitive.Effect


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
    => Def r a
    -> (a -> Def r ())
    -> (a -> Def r b)
    -> Def r b
bracket alloc dealloc use = send $ Bracket alloc dealloc use id
{-# INLINE bracket #-}


runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Def r x -> IO x)
    -> Def (Resource ': r) a
    -> Def r a
runResource finish = interpret $ \case
  Bracket alloc dealloc use k -> fmap k . sendM $
    let runIt :: Def (Resource ': r) x -> IO x
        runIt = finish . runResource' finish
     in X.bracket
          (runIt alloc)
          (runIt . dealloc)
          (runIt . use)
{-# INLINE runResource #-}


runResource'
    :: Member (Lift IO) r
    => (∀ x. Def r x -> IO x)
    -> Def (Resource ': r) a
    -> Def r a
runResource' = runResource
{-# NOINLINE runResource' #-}

