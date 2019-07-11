{-# OPTIONS_GHC -fdefer-type-errors -fno-warn-deferred-type-errors #-}

module LegitimateTypeErrorSpec where

import Polysemy
import Test.Hspec
import Test.ShouldNotTypecheck

wrongEmbed :: Member (Embed IO) r => Sem r ()
wrongEmbed = embed putStrLn

wrongReturn :: Sem (e ': r) () -> Sem r ()
wrongReturn = reinterpret undefined



spec :: Spec
spec = do
  describe "Legitimate type errors" $ do
    it "should be caused by `embed`ing an unsaturated function" $
        shouldNotTypecheck wrongEmbed

    it "should be caused by giving a bad type to reinterpret" $
        shouldNotTypecheck wrongReturn

