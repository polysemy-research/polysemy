-- | Description: 'Fail' effect
module Polysemy.Fail.Type where

------------------------------------------------------------------------------
-- | This effect abstracts the concept of 'Control.Monad.Fail.MonadFail',
-- which is a built-in mechanism that converts pattern matching errors to
-- calls to the current monad's instance of that class.
--
-- The instance defined in "Polysemy.Internal" uses this effect to catch
-- those errors.
newtype Fail m a = Fail String
