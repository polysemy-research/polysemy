module Polysemy.Opaque (
  -- * Effect
  Opaque(..),

  -- * Interpreters
  toOpaque,
  fromOpaque,
  ) where

import Polysemy

-- | An effect newtype meant to be used to wrap polymorphic effect variables to
-- prevent them from jamming up resolution of 'Polysemy.Member'.
-- For example, consider:
--
-- @
-- badPut :: 'Sem' (e ': 'Polysemy.State.State' () ': r) ()
-- badPut = 'Polysemy.State.put' () -- error
-- @
--
-- This fails to compile. This is because @e@ /could/ be
-- @'Polysemy.State.State' ()@' -- in which case the 'Polysemy.State.put'
-- should target it instead of the concretely provided
-- @'Polysemy.State.State' ()@; as the compiler can't know for sure which effect
-- should be targeted, the program is rejected.
-- There are various ways to resolve this, including using 'raise' or
-- 'Polysemy.Membership.subsumeUsing'. 'Opaque' provides another way:
--
-- @
-- okPut :: 'Sem' (e ': 'Polysemy.State.State' () ': r) ()
-- okPut = 'fromOpaque' ('Polysemy.State.put' ()) -- OK
-- @
--
-- 'Opaque' is most useful as a tool for library writers, in the case where some
-- function of the library requires the user to work with an effect stack
-- containing some polymorphic effect variables. By wrapping the polymorphic
-- effect variables using 'Opaque', users of the function can use effects as
-- normal, without having to use 'raise' or 'Polysemy.Membership.subsumeUsing'
-- in order to have 'Polysemy.Member' resolve.
--
-- @since TODO
newtype Opaque (e :: Effect) m a = Opaque (e m a)

-- | Wrap 'Opaque' around the top effect of the effect stack
toOpaque :: Sem (e ': r) a -> Sem (Opaque e ': r) a
toOpaque = rewrite Opaque
{-# INLINE toOpaque #-}

-- | Unwrap 'Opaque' around the top effet of the effect stack
fromOpaque :: Sem (Opaque e ': r) a -> Sem (e ': r) a
fromOpaque = rewrite (\(Opaque e) -> e)
{-# INLINE fromOpaque #-}
