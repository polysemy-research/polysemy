module Polysemy.Exceptional where

import Polysemy
import Polysemy.Error
import qualified Control.Exception as X
import Polysemy.Interpretation
import qualified System.IO as SysIO

data Exceptional exc eff :: Effect where
  Exceptional :: forall exc eff m a. eff m a -> Exceptional e m (Either exc a)

trying :: forall exc eff r a
        . Member (Exceptional exc eff) r
       => Eff (eff ': r) a -> Eff r (Either exc a)
trying =
    runError
  . interpretH (\e -> propagate (Exceptional @exc e) >>= fromEither)
  . raiseUnder

newtype Stop e :: Effect where
  Stop :: e -> Stop e m void

makeSem ''Stop

runExceptional
  :: (   forall t rInitial x
       . Traversable t
      => exc (Sem rInitial) x
      -> Sem (RunH (Sem rInitial) t (Exceptional exc eff) r ': Stop exc ': r) a
     )
  -> InterpreterFor (Exceptional exc eff)
runExceptional h = interpretH $ \(Exceptional e) ->
    h e
  & insertAt @2
  & subsume
  & rewrite (\(Stop e) -> Throw e)
  & runError
