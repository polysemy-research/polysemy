{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

module LegitimateTypeErrorSpec where

import Polysemy
import Test.Hspec
import Test.ShouldNotTypecheck

wrongLift :: Member (Lift IO) r => Sem r ()
wrongLift = sendM putStrLn

wrongReturn :: Sem (e ': r) () -> Sem r ()
wrongReturn = reinterpret undefined



spec :: Spec
spec = do
  describe "Legitimate type errors" $ do
    it "should be caused by `sendM`ing an unsaturated function" $
        shouldNotTypecheck wrongLift

    it "should be caused by giving a bad type to reinterpret" $
        shouldNotTypecheck wrongReturn

