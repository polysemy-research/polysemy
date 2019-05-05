{-# LANGUAGE DefaultSignatures     #-}

{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Effect where

import Data.Coerce
import Data.Functor.Identity


------------------------------------------------------------------------------
-- | The class for semantic effects.
--
-- An effect @e@ is a type @e m a@, where the other types are given by:
--
-- * The @m@ type variable corresponds to a monad, which will eventually be
-- instantiated at 'Polysemy.Sem'---meaning it is capable of encoding
-- arbitrary other effects.
--
-- * The @a@ type is handled automatically and uninteresting.
--
-- The type @e m@ must be a 'Functor', but this instance can always be given
-- for free via the @-XDeriveFunctor@ language extension. Often this instance
-- must be derived as a standalone (@-XStandaloneDeriving@):
--
-- @
-- deriving instance Functor (MyEffect m)
-- @
--
-- If the effect doesn't use @m@ whatsoever it is said to be /first-order/.
-- First-order effects can be given an instance of 'Effect' for free with
-- @-XDeriveAnyClass@.
--
-- @
-- deriving instance Effect MyEffect
-- @
class Effect e where
  -- | Provide a specialised version of 'fmap' to work around versions of GHC
  -- that cannot express the quantified constraint '(∀ m. Functor m => Functor (e m))'
  --
  -- This must always be equal to 'fmap', and can be removed once GHC 8.6 is the
  -- minimum version supported by this library
  fmap' :: (a -> b) -> (e m a -> e m b)

  default fmap' :: Functor (e m) => (a -> b) -> (e m a -> e m b)
  fmap' = fmap
  {-# INLINE fmap' #-}

  -- | Higher-order effects require the ability to distribute state from other
  -- effects throughout themselves. This state is given by an initial piece of
  -- state @s ()@, and a distributive law that describes how to move the state
  -- through an effect.
  --
  -- When the effect @e@ has multiple computations in the @m@ monad, 'weave'
  -- defines the semantics for how these computations will view with the state:
  --
  -- * If the resulting state from one computation is fed to another, the second
  -- computation will see the state that results from the first computation.
  --
  -- * If instead it is given the intial state, both computations will see the
  -- same state, but the result of (at least) one will necessarily be ignored.
  weave
      :: (Functor s, Functor m, Functor n)
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)

  -- | When @e@ is first order, 'weave' can be given for free.
  default weave
      :: ( Coercible (e m (s a)) (e n (s a))
         , Functor s
         , Functor m
         , Functor n
         )
      => s ()
      -> (∀ x. s (m x) -> n (s x))
      -> e m a
      -> e n (s a)
  weave s _ = coerce . fmap' (<$ s)
  {-# INLINE weave #-}

  -- | Lift a natural transformation from @m@ to @n@ over the effect. 'hoist'
  -- should be defined as 'defaultHoist', but can be hand-written if the
  -- default performance isn't sufficient.
  hoist
        :: ( Functor m
           , Functor n
           )
        => (∀ x. m x -> n x)
        -> e m a
        -> e n a

  -- | When @e@ is first order, 'hoist' be given for free.
  default hoist
      :: ( Coercible (e m a) (e n a)
         , Functor m
         )
      => (∀ x. m x -> n x)
      -> e m a
      -> e n a
  hoist _ = coerce
  {-# INLINE hoist #-}


------------------------------------------------------------------------------
-- | A default implementation of 'hoist'. Particularly performance-sensitive
-- effects should give a hand-written their own implementation of 'hoist'.
defaultHoist
      :: ( Functor m
         , Functor n
         , Effect e
         )
      => (∀ x. m x -> n x)
      -> e m a
      -> e n a
defaultHoist f
  = fmap' runIdentity
  . weave (Identity ())
          (fmap Identity . f . runIdentity)
{-# INLINE defaultHoist #-}

