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

It's like `freer-simple` but more powerful and 700x faster.

It's like `fused-effects` but with an order of magnitude less boilerplate.


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

Console effect:

```haskell
{-# LANGAUGE TemplateHaskell #-}

import Polysemy

data Console m a where
  GetLine :: Console m String
  PutLine :: String -> Console m ()

makeSemantic ''Console

runConsoleIO :: Member (Lift IO) r => Semantic (Console ': r) a -> Semantic r a
runConsoleIO = interpret $ \case
  GetLine     -> sendM getLine
  PutLine msg -> sendM $ putStrLn msg
```


Resource effect:

```haskell
{-# LANGUAGE TemplateHaskell #-}

import qualified Control.Exception as X
import           Polysemy

data Resource m a where
  Bracket :: m a -> (a -> m ()) -> (a -> m b) -> Resource m b

makeSemantic ''Resource

runResource
    :: forall r a
     . Member (Lift IO) r
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
runResource finish = interpretH $ \case
  Bracket alloc dealloc use -> do
    a <- runT  alloc
    d <- bindT dealloc
    u <- bindT use

    let runIt :: Semantic (Resource ': r) x -> IO x
        runIt = finish .@ runResource

    sendM $ X.bracket (runIt a) (runIt . d) (runIt . u)
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
    => (∀ x. Semantic r x -> IO x)
    -> Semantic (Resource ': r) a
    -> Semantic r a
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

