{-# LANGUAGE CPP              #-}
{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeApplications #-}

{-# OPTIONS_GHC -O2 #-}
{-# OPTIONS_GHC -dsuppress-idinfo -dsuppress-coercions #-}


#if __GLASGOW_HASKELL__ < 804
{-# OPTIONS_GHC -fplugin=Test.Inspection.Plugin #-}
#endif

module FusionSpec where

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State.Strict as S
import           Polysemy.Error
import           Polysemy.Internal
import           Polysemy.Internal.Combinators
import           Polysemy.Internal.Union
import           Polysemy.State
import           Test.Hspec
import           Test.Inspection


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure e) = error e

shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` isSuccess


spec :: Spec
spec = parallel $ do
  describe "fusion" $ do
-- #if __GLASGOW_HASKELL__ >= 807
--     -- TODO: Investigate why this test fails mysteriously on GHC < 8.6
--     it "Union proofs should simplify" $ do
--       shouldSucceed $(inspectTest $ 'countDown `hasNoType` ''SNat)
-- #endif

    it "internal uses of StateT should simplify" $ do
      shouldSucceed $(inspectTest $ 'countDown `doesNotUse` ''S.StateT)
      shouldSucceed $(inspectTest $ 'jank      `doesNotUse` ''S.StateT)

    it "internal uses of ExceptT should simplify" $ do
      shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` ''E.ExceptT)

    it "`runState . reinterpret` should fuse" $ do
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'reinterpret)
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'hoist)

    it "who needs Sem even?" $ do
      shouldSucceed $(inspectTest $ 'countDown `doesNotUse` 'Sem)
      shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'Sem)
      shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` 'Sem)

-- #if __GLASGOW_HASKELL__ >= 807
--     it "who needs Weaving even?" $ do
--       shouldSucceed $(inspectTest $ 'jank `doesNotUse` 'Weaving)
--       shouldSucceed $(inspectTest $ 'countDown `doesNotUse` 'Weaving)
-- #if __GLASGOW_HASKELL__ >= 810
--       shouldSucceed $(inspectTest $ 'tryIt `doesNotUse` 'Weaving)
-- #endif
-- #endif


go :: Sem '[State Int] Int
go = do
  n <- get
  if n <= 0
     then pure n
     else do
       put (n-1)
       go


tryIt :: Either Bool String
tryIt = run . runError @Bool $
  catch @Bool
    (throw False)
    (\_ -> pure "hello")


countDown :: Int -> Int
countDown start = fst $ run $ runState start go


jank :: Int -> Int
jank start = fst $ run $ runState start $ go

