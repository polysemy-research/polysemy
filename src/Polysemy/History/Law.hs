module Polysemy.History.Law where

import Polysemy
import Polysemy.Law
import Polysemy.State
import Polysemy.History


------------------------------------------------------------------------------
-- | A collection of laws that show a `State` interpreter is correct.
prop_lawfulHistory
    :: (Eq s, Show s, Arbitrary s)
    => InterpreterFor (History s) '[State s]
    -> Property
prop_lawfulHistory i12n = conjoin
  [ runLaw i12n law_undoes
  , runLaw i12n law_redoes
  , runLaw i12n law_forgotten
  ]


law_undoes
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (History s) '[State s]
law_undoes =
  Law (\s -> run . evalState @s s)
    "put %1 >> undo >> get"
    (\s -> put @s s >> undo @s >> get @s)
    "get"
    (\_ -> get @s)


law_redoes
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (History s) '[State s]
law_redoes =
  Law (\s -> run . evalState @s s)
    "put %1 >> undo >> redo >> get"
    (\s -> put @s s >> undo @s >> redo @s >> get @s)
    "put %1 >> get"
    (\s -> put @s s >> get @s)


law_forgotten
    :: forall s
     . (Eq s, Arbitrary s, Show s)
    => Law (History s) '[State s]
law_forgotten =
  Law (\s -> run . evalState @s s)
    "put %1 >> undo >> redo >> get"
    (\s s' -> put @s s >> putAndForget @s s' >> undo @s >> redo @s >> get @s)
    "put %1 >> get"
    (\s _ -> put @s s >> get @s)

