module Polysemy.Final.Type where

-----------------------------------------------------------------------------
-- | An effect for embedding higher-order actions in the final target monad
-- of the effect stack.
--
-- This is very useful for writing interpreters that interpret higher-order
-- effects in terms of the final monad.
--
-- 'Final' is more powerful than 'Embed', but is also less flexible
-- to interpret (compare 'Polysemy.Embed.runEmbedded' with 'finalToFinal').
-- If you only need the power of 'embed', then you should use 'Embed' instead.
--
-- /Beware/: 'Final' actions are interpreted as actions of the final monad,
-- and the effectful state visible to
-- 'withWeavingToFinal' \/ 'withStrategicToFinal'
-- \/ 'interpretFinal'
-- is that of /all interpreters run in order to produce the final monad/.
--
-- This means that any interpreter built using 'Final' will /not/
-- respect local/global state semantics based on the order of
-- interpreters run. You should signal interpreters that make use of
-- 'Final' by adding a @-'Final'@ suffix to the names of these.
--
-- State semantics of effects that are /not/
-- interpreted in terms of the final monad will always
-- appear local to effects that are interpreted in terms of the final monad.
--
-- State semantics between effects that are interpreted in terms of the final monad
-- depend on the final monad. For example, if the final monad is a monad transformer
-- stack, then state semantics will depend on the order monad transformers are stacked.
--
-- @since 1.2.0.0
newtype Final m z a where
  WithWeavingToFinal
    :: ThroughWeavingToFinal m z a
    -> Final m z a

-----------------------------------------------------------------------------
-- | This represents a function which produces
-- an action of the final monad @m@ given:
--
--   * The initial effectful state at the moment the action
--     is to be executed.
--
--   * A way to convert @z@ (which is typically @'Sem' r@) to @m@ by
--     threading the effectful state through.
--
--   * An inspector that is able to view some value within the
--     effectful state if the effectful state contains any values.
--
-- A @'Polysemy.Internal.Union.Weaving'@ provides these components,
-- hence the name 'ThroughWeavingToFinal'.
--
-- @since 1.2.0.0
type ThroughWeavingToFinal m z a =
     forall f
   . Functor f
  => f ()
  -> (forall x. f (z x) -> m (f x))
  -> (forall x. f x -> Maybe x)
  -> m (f a)
