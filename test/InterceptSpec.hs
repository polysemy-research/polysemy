module InterceptSpec where

import Polysemy
import Polysemy.Resource
import Polysemy.Trace
import Test.Hspec


withTraceLogging :: Members '[Trace] r => Sem r a -> Sem r a
withTraceLogging = intercept $ \case
  Trace msg -> do
    trace $ "logged " <> msg
    trace msg

spec :: Spec
spec = describe "intercept" $ do
  it "should weave through embedded computations" $ do
    let (msgs, ()) = run
                   . runTraceList
                   . runResource
                   . withTraceLogging $ do
          trace "outside"
          bracket (pure ()) pure (const $ trace "inside")
    msgs `shouldBe` ["logged outside", "outside", "logged inside", "inside"]

