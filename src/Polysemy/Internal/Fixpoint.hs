{-# OPTIONS_HADDOCK not-home #-}

module Polysemy.Internal.Fixpoint where

------------------------------------------------------------------------------
-- | An effect for providing 'Control.Monad.Fix.mfix'.
newtype Fixpoint m a where
  Fixpoint :: (a -> m a) -> Fixpoint m a


------------------------------------------------------------------------------
-- | The error used in 'Polysemy.Fixpoint.fixpointToFinal',
-- 'Polysemy.Fixpoint.runFixpoint' and 'Polysemy.Fixpoint.runFixpointM'
-- when the result of a failed computation
-- is recursively used and somehow visible. You may use this for your own
-- 'Fixpoint' interpreters. The argument should be the name of the interpreter.
bomb :: String -> a
bomb str = error $
    str ++ ": Internal computation failed.\
            \ This is likely because you have tried to recursively use\
            \ the result of a failed computation in an action\
            \ whose effect may be observed even though the computation failed.\
            \ It's also possible that you're using an interpreter\
            \ that uses 'weave' improperly.\
            \ See documentation for more information."
