{-# LANGUAGE TemplateHaskell #-}
module Polysemy.Exceptional where

import Data.Function ((&))
import Polysemy
import Polysemy.Error
import qualified Control.Exception as X
import Polysemy.Interpretation
import qualified System.IO as SysIO

data Exceptional exc eff :: Effect where
  Exceptional :: forall exc eff m a. eff m a -> Exceptional exc eff m (Either exc a)

trying :: forall exc eff r a
        . Member (Exceptional exc eff) r
       => Sem (eff ': r) a -> Sem r (Either exc a)
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
      => eff (Sem rInitial) x
      -> Sem (RunH (Sem rInitial) t (Exceptional exc eff) r ': Stop exc ': r) x
     )
  -> InterpreterFor (Exceptional exc eff) r
runExceptional h = interpretH $ \(Exceptional e) ->
    h e
  & insertAt @2
  & subsume @(RunH)
  & rewrite (\(Stop e) -> Throw e)
  & runError
