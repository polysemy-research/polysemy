module KnownRowSpec where

import Polysemy
import Polysemy.Error
import Polysemy.State
import Polysemy.Internal
import Polysemy.Internal.Union

import Test.Hspec

-- | A variant of 'runState' that uses 'stateToIO' if @r@ contains @Embed IO@.
-- (Can also be extended to check for @Final IO@)
runState' :: forall s r a. KnownRow r => s -> Sem (State s ': r) a -> Sem r (s, a)
runState' s sem = case tryMembership @(Embed IO) of
  Just proof -> subsumeUsing proof (stateToIO s (raiseUnder sem))
  _          -> runState s sem


test :: (Member (Error ()) r, KnownRow r)
     => Sem r String
test = fmap fst $ runState' "" $ do
  put "local state"
  _ <- (put "global state" >> throw ()) `catch` \() -> return ()
  return ()

spec :: Spec
spec = parallel $ describe "tryMembership" $ do
  it "should return a valid proof when the targeted \
     \ effect is part of the row" $ do
    res <- runM . runError @() $ test
    res `shouldBe` Right "global state"
  it "should not return a valid proof when the targeted \
     \ effect is not part of the row" $ do
    let res = run . runError @() $ test
    res `shouldBe` Right "local state"
