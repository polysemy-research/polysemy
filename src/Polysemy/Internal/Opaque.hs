{-# OPTIONS_HADDOCK not-home #-}

-- | The auxiliary effect 'Opaque' used by interpreters of 'Polysemy.Scoped.Scoped'
module Polysemy.Internal.Opaque where

import Polysemy.Internal.Kind

-- | An effect newtype meant to be used to wrap polymorphic effect variables to
-- prevent them from jamming up resolution of 'Polysemy.Member'.
--
-- 'Opaque' is given special treatment by 'Member' so that
-- @'Polysemy.Member' e ('Opaque' q ': r)@ will never consider targeting
-- @'Opaque' q@ unless @e@ is known to be exactly @'Opaque' q@.
--
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
-- in order to have 'Polysemy.Member' resolve. The various interpreters of
-- 'Polysemy.Scoped.Scoped' are examples of such usage of 'Opaque'.
--
-- @since 1.9.0.0
newtype Opaque (e :: Effect) m a = Opaque (e m a)
