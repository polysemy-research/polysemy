{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Polysemy.State.Law where

import Polysemy
import Polysemy.Law
import Polysemy.State
import Control.Applicative
import Control.Arrow


------------------------------------------------------------------------------
-- | A collection of laws that show a `State` interpreter is correct.
prop_lawfulState
    :: forall r s
     . (Eq s, Show s, Arbitrary s, MakeLaw (State s) r)
    => InterpreterFor (State s) r
    -> Property
prop_lawfulState i12n = conjoin
  [ runLaw i12n lawPutTwice
  , runLaw i12n lawGetTwice
  , runLaw i12n lawGetPutGet
  ]



lawPutTwice
    :: forall s r
     . (Eq s, Arbitrary s, Show s, MakeLaw (State s) r)
    => Law (State s) r
lawPutTwice =
  mkLaw
    "put %1 >> put %2 >> get"
    (\s s' -> put @s s >> put @s s' >> get @s)
    "put %2 >> get"
    (\_ s' ->             put @s s' >> get @s)

lawGetTwice
    :: forall s r
     . (Eq s, Arbitrary s, Show s, MakeLaw (State s) r)
    => Law (State s) r
lawGetTwice =
  mkLaw
    "liftA2 (,) get get"
    (liftA2 (,) (get @s) (get @s))
    "(id &&& id) <$> get"
    ((id &&& id) <$> get @s)

lawGetPutGet
    :: forall s r
     . (Eq s, Arbitrary s, Show s, MakeLaw (State s) r)
    => Law (State s) r
lawGetPutGet =
  mkLaw
    "get >>= put >> get"
    (get @s >>= put @s >> get @s)
    "get"
    (get @s)

