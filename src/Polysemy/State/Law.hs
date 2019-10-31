module Polysemy.State.Law where

import Polysemy
import Polysemy.Law
import Polysemy.State
import Control.Applicative
import Control.Arrow


------------------------------------------------------------------------------
-- | A collection of laws that show a `State` interpreter is correct.
runStateLaws
    :: (Eq s, Show s, Arbitrary s)
    => InterpreterFor (State s) '[]
    -> Property
runStateLaws i12n = conjoin
  [ runLaw i12n lawPutTwice
  , runLaw i12n lawGetTwice
  , runLaw i12n lawGetPutGet
  ]

runStateLawsIO
    :: (Eq s, Show s, Arbitrary s)
    => InterpreterFor (State s) '[Embed IO]
    -> Property
runStateLawsIO i12n = conjoin
  [ runLaw i12n lawPutTwiceIO
  , runLaw i12n lawGetTwiceIO
  , runLaw i12n lawGetPutGetIO
  ]


lawPutTwice
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawPutTwice =
  Law run
    "put %1 >> put %2 >> get"
    (\s s' -> put @s s >> put @s s' >> get @s)
    "put %2 >> get"
    (\_ s' ->             put @s s' >> get @s)

lawPutTwiceIO
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[Embed IO]
lawPutTwiceIO =
  LawIO runM
    "put %1 >> put %2 >> get"
    (\s s' -> put @s s >> put @s s' >> get @s)
    "put %2 >> get"
    (\_ s' ->             put @s s' >> get @s)


lawGetTwice
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawGetTwice =
  Law run
    "liftA2 (,) get get"
    (liftA2 (,) (get @s) (get @s))
    "(id &&& id) <$> get"
    ((id &&& id) <$> get @s)

lawGetTwiceIO
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[Embed IO]
lawGetTwiceIO =
  LawIO runM
    "liftA2 (,) get get"
    (liftA2 (,) (get @s) (get @s))
    "(id &&& id) <$> get"
    ((id &&& id) <$> get @s)


lawGetPutGet
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[]
lawGetPutGet =
  Law run
    "get >>= put >> get"
    (get @s >>= put @s >> get @s)
    "get"
    (get @s)


lawGetPutGetIO
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (State s) '[Embed IO]
lawGetPutGetIO =
  LawIO runM
    "get >>= put >> get"
    (get @s >>= put @s >> get @s)
    "get"
    (get @s)

