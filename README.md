# polysemy

[![Build Status](https://api.travis-ci.org/isovector/polysemy.svg?branch=master)](https://travis-ci.org/isovector/polysemy)
[![Hackage](https://img.shields.io/hackage/v/polysemy.svg?logo=haskell)](https://hackage.haskell.org/package/polysemy)

## Dedication

> The word 'good' has many meanings. For example, if a man were to shoot his
> grandmother at a range of five hundred yards, I should call him a good shot,
> but not necessarily a good man.
>
> Gilbert K. Chesterton


## Overview

`polysemy` is a library for writing high-power, low-boilerplate, zero-cost,
domain specific languages. It allows you to separate your business logic from
your implementation details. And in doing so, `polysemy` lets you turn your
implementation code into reusable library code.

It's like `mtl` but composes better, requires less boilerplate, and avoids the
O(n^2) instances problem.

It's like `freer-simple` but more powerful and 35x faster.

It's like `fused-effects` but with an order of magnitude less boilerplate.

Additionally, unlike `mtl`, `polysemy` has no functional dependencies, so you
can use multiple copies of the same effect. This alleviates the need for ~~ugly
hacks~~band-aids like [classy
lenses](http://hackage.haskell.org/package/lens-4.17.1/docs/Control-Lens-TH.html#v:makeClassy),
the [`ReaderT`
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) and
nicely solves the [trouble with typed
errors](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html).

Concerned about type inference? Check out
[polysemy-plugin](https://github.com/isovector/polysemy/tree/master/polysemy-plugin),
which should perform just as well as `mtl`'s!


## Features

* *Effects are higher-order,* meaning it's trivial to write `bracket` and `local`
    as first-class effects.
* *Effects are low-boilerplate,* meaning you can create new effects in a
    single-digit number of lines. New interpreters are nothing but functions and
    pattern matching.
* *Effects are zero-cost,* meaning that GHC<sup>[1](#fn1)</sup> can optimize
    away the entire abstraction at compile time.


<sup><a name="fn1">1</a></sup>: Unfortunately this is not true in GHC 8.6.3, but
will be true as soon as [my patch](https://gitlab.haskell.org/ghc/ghc/merge_requests/668/) lands.


## Examples

Make sure you read the [Necessary Language
Extensions](https://github.com/isovector/polysemy#necessary-language-extensions)
before trying these yourself!

Teletype effect:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, DataKinds, FlexibleContexts, GADTs, LambdaCase,
             PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications,
             TypeOperators, TypeFamilies, UnicodeSyntax
#-}

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

runTeletypeIO :: Member (Lift IO) r => Sem (Teletype ': r) a -> Sem r a
runTeletypeIO = interpret $ \case
  ReadTTY      -> sendM getLine
  WriteTTY msg -> sendM $ putStrLn msg

runTeletypeBoring :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypeBoring i
  = runFoldMapOutput pure
  . runListInput i
  . reinterpret2 \case
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg

-- Continuations so we can build up chains of behavior
echoK :: Member Teletype r => Sem r () -> Sem r ()
echoK k = do
  a <- readTTY
  when (a == "") $ pure ()
  when (a /= "") do
    writeTTY a
    k

revK :: Member Teletype r => Sem r () -> Sem r ()
revK k = do
  a <- readTTY
  when (a == "") $ pure ()
  when (a /= "") do
    writeTTY (reverse a)
    k

-- Fixpoints

echo :: Member Teletype r => Sem r ()
echo = fix echoK

rev :: Member Teletype r => Sem r ()
rev = fix revK

alternating :: Member Teletype r => Sem r ()
alternating = fix (echoK . revK)

-- Let's pretend
echoBoring :: [String] -> Sem '[] ([String], ())
echoBoring = flip runTeletypeBoring echo

revBoring :: [String] -> Sem '[] ([String], ())
revBoring = flip runTeletypeBoring rev

alternatingBoring :: [String] -> Sem '[] ([String], ())
alternatingBoring = flip runTeletypeBoring alternating

boring :: [String] -> [[String]]
boring = map (fst . run) . sequence [echoBoring, revBoring, alternatingBoring]

-- Now let's do things
echoIO :: Sem '[Lift IO] ()
echoIO = runTeletypeIO echo

revIO :: Sem '[Lift IO] ()
revIO = runTeletypeIO rev

alternatingIO :: Sem '[Lift IO] ()
alternatingIO = runTeletypeIO alternating

-- cycle echo and reverse until a blank line
main :: IO ()
main = runM alternatingIO
```


Resource effect:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, DataKinds, FlexibleContexts, GADTs, LambdaCase,
             PolyKinds, RankNTypes, ScopedTypeVariables, TypeApplications,
             TypeOperators, TypeFamilies, UnicodeSyntax
#-}

import Prelude hiding (throw, catch, bracket)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import qualified Control.Exception as X

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

data Resource m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b

makeSem ''Teletype
makeSem ''Resource


runTeletypeIO :: Member (Lift IO) r => Sem (Teletype ': r) a -> Sem r a
runTeletypeIO = interpret $ \case
  ReadTTY      -> sendM getLine
  WriteTTY msg -> sendM $ putStrLn msg

runTeletypeBoring :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypeBoring i
  = runFoldMapOutput pure
  . runListInput i
  . reinterpret2 \case
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg


runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Sem r x -> IO x)
    -> Sem (Resource ': r) a
    -> Sem r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let runIt :: Sem (Resource ': r) x -> IO x
        runIt = finish .@ runResource

    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)

data CustomException = ThisException | ThatException deriving Show
instance X.Exception CustomException

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch work $ \e -> writeTTY ("Caught " ++ show e)
  where work = bracket (readTTY) (const $ writeTTY "exiting bracket") $ \input -> do
          writeTTY "entering bracket"
          when (input == "explode") $ throw ThisException
          writeTTY input
          when (input == "weird stuff") $ throw ThatException
          writeTTY "no exceptions"

boringProgram :: IO (Either CustomException ([String], ()))
boringProgram = (runM .@ runResource) . runError @CustomException . runTeletypeBoring ["words"] $ program

boringExplosion :: IO (Either CustomException ([String], ()))
boringExplosion = (runM .@ runResource) . runError @CustomException . runTeletypeBoring ["explode"] $ program

ioProgram :: IO (Either CustomException ())
ioProgram = (runM .@ runResource .@@ runErrorInIO @CustomException) . runTeletypeIO $ program
```

Easy.


## Friendly Error Messages

Free monad libraries aren't well known for their ease-of-use. But following in
the shoes of `freer-simple`, `polysemy` takes a serious stance on providing
helpful error messages.

For example, the library exposes both the `interpret` and `interpretH`
combinators. If you use the wrong one, the library's got your back:

```haskell
runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Sem r x -> IO x)
    -> Sem (Resource ': r) a
    -> Sem r a
runResource finish = interpret $ \case
  ...
```

makes the helpful suggestion:

```
    • 'Resource' is higher-order, but 'interpret' can help only
      with first-order effects.
      Fix:
        use 'interpretH' instead.
    • In the expression:
        interpret
          $ \case
```

Likewise it will give you tips on what to do if you forget a `TypeApplication`
or forget to handle an effect.

Don't like helpful errors? That's OK too --- just flip the `error-messages` flag
and enjoy the raw, unadulterated fury of the typesystem.


## Necessary Language Extensions

You're going to want to stick all of this into your `package.yaml` file.

```yaml
  ghc-options: -O2 -flate-specialise -fspecialise-aggressively
  default-extensions:
    - DataKinds
    - FlexibleContexts
    - GADTs
    - LambdaCase
    - PolyKinds
    - RankNTypes
    - ScopedTypeVariables
    - TypeApplications
    - TypeOperators
    - TypeFamilies
```

