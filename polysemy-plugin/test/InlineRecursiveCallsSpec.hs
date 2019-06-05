{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -ddump-simpl -dsuppress-coercions -dsuppress-uniques -dsuppress-idinfo #-}


module InlineRecursiveCallsSpec
  ( spec
  ) where

import qualified Control.Monad.Trans.State as S
import           Data.Tuple
import           Polysemy.Internal
import           Polysemy.Internal.Effect
import           Polysemy.Internal.Union
import           Test.Hspec
import           Test.Inspection


spec :: Spec
spec = describe "inlining recursive calls" $ do
  it "should explicitly break recursion" $ do
    shouldSucceed $(inspectTest $ 'recursive === 'mutual)


isSuccess :: Result -> Bool
isSuccess (Success _) = True
isSuccess (Failure e) = error e


shouldSucceed :: Result -> Expectation
shouldSucceed r = r `shouldSatisfy` isSuccess


------------------------------------------------------------------------------
recursive
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
recursive f s (Sem m) = Sem $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ())
                    (uncurry $ recursive f)
                    (Just . snd)
            $ x
        Right (Yo e z _ y _) ->
          fmap (y . (<$ z)) $ S.mapStateT (usingSem k) $ f e


------------------------------------------------------------------------------
mutual
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
mutual f s (Sem m) = Sem $ \k ->
  fmap swap $ flip S.runStateT s $ m $ \u ->
    case decomp u of
        Left x -> S.StateT $ \s' ->
          k . fmap swap
            . weave (s', ())
                    (uncurry $ mutual2 f)
                    (Just . snd)
            $ x
        Right (Yo e z _ y _) ->
          fmap (y . (<$ z)) $ S.mapStateT (usingSem k) $ f e
{-# INLINE mutual #-}

mutual2
    :: (∀ x m. e m x -> S.StateT s (Sem r) x)
    -> s
    -> Sem (e ': r) a
    -> Sem r (s, a)
mutual2 = mutual
{-# NOINLINE mutual2 #-}

