{-# LANGUAGE NoPolyKinds #-}

module Polysemy.Internal.Lift where


------------------------------------------------------------------------------
-- | An effect which allows a regular 'Monad' @m@ into the 'Polysemy.Sem'
-- ecosystem. Monadic actions in @m@ can be lifted into 'Polysemy.Sem' via
-- 'Polysemy.sendM'.
--
-- For example, you can use this effect to lift 'IO' actions directly into
-- 'Polysemy.Sem':
--
-- @
-- 'Polysemy.sendM' (putStrLn "hello") :: 'Polysemy.Member' ('Polysemy.Lift' IO) r => 'Polysemy.Sem' r ()
-- @
--
-- That being said, you lose out on a significant amount of the benefits of
-- 'Polysemy.Sem' by using 'Polysemy.sendM' directly in application code; doing
-- so will tie your application code directly to the underlying monad, and
-- prevent you from interpreting it differently. For best results, only use
-- 'Lift' in your effect interpreters.
--
-- Consider using 'Polysemy.Trace.trace' and 'Polysemy.Trace.runTraceIO' as
-- a substitute for using 'putStrLn' directly.
newtype Lift m (z :: * -> *) a where
  Lift :: { unLift :: m a } -> Lift m z a

