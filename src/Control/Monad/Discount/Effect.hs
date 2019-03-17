{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE UnicodeSyntax         #-}

module Control.Monad.Discount.Effect where

import Data.Coerce
import Data.Functor.Identity


class (∀ m. Functor m => Functor (e m)) => Effect e where
  weave
      :: (Functor s, Functor m)
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)

  default weave
      :: ( Coercible (e m (s a)) (e n (s a))
         , Functor s
         , Functor m
         )
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)
  weave s _ = coerce . fmap (<$ s)
  {-# INLINE weave #-}

  hoist
        :: ( Functor m
           , Functor n
           )
        => (∀ x. m x -> n x)
        -> e m a
        -> e n a
  hoist f = fmap runIdentity
          . weave (Identity ())
                  (fmap Identity . f . runIdentity)
  {-# INLINE hoist #-}

