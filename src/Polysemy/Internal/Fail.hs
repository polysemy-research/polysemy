module Polysemy.Internal.Fail where

newtype Fail m a = Fail String
