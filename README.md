<p align="center">
<img src="https://raw.githubusercontent.com/isovector/polysemy/master/polysemy.png" alt="Polysemy" title="Polysemy">
</p>

<p>&nbsp;</p>

# polysemy

[![Build Status](https://api.travis-ci.org/polysemy-research/polysemy.svg?branch=master)](https://travis-ci.org/polysemy-research/polysemy)
[![Hackage](https://img.shields.io/hackage/v/polysemy.svg?logo=haskell&label=polysemy)](https://hackage.haskell.org/package/polysemy)
[![Hackage](https://img.shields.io/hackage/v/polysemy-plugin.svg?logo=haskell&label=polysemy-plugin)](https://hackage.haskell.org/package/polysemy-plugin)

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
hacks~~ band-aids like [classy
lenses](http://hackage.haskell.org/package/lens-4.17.1/docs/Control-Lens-TH.html#v:makeClassy),
the [`ReaderT`
pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern) and
nicely solves the [trouble with typed
errors](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html).

Concerned about type inference? Check out
[polysemy-plugin](https://github.com/isovector/polysemy/tree/master/polysemy-plugin),
which should perform just as well as `mtl`'s! Add `polysemy-plugin` to your package.yaml
or .cabal file's dependencies section to use. Then turn it on with a pragma in your source-files:

```haskell
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
```
Or by adding `-fplugin=Polysemy.Plugin` to your package.yaml/.cabal file `ghc-options` section.


## Features

* *Effects are higher-order,* meaning it's trivial to write `bracket` and `local`
    as first-class effects.
* *Effects are low-boilerplate,* meaning you can create new effects in a
    single-digit number of lines. New interpreters are nothing but functions and
    pattern matching.
* *Effects are zero-cost,* meaning that GHC<sup>[1](#fn1)</sup> can optimize
    away the entire abstraction at compile time.


<sup><a name="fn1">1</a></sup>: Unfortunately this is not true in GHC 8.6.3, but
will be true in GHC 8.10.1.


## Examples

Make sure you read the [Necessary Language
Extensions](https://github.com/isovector/polysemy#necessary-language-extensions)
before trying these yourself!

Teletype effect:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds #-}

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Lift IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret $ \case
  ReadTTY      -> sendM getLine
  WriteTTY msg -> sendM $ putStrLn msg

runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  = runOutputMonoid pure  -- For each WriteTTY in our program, consume an output by appending it to the list in a ([String], a)
  . runInputList i         -- Treat each element of our list of strings as a line of input
  . reinterpret2 \case     -- Reinterpret our effect in terms of Input and Output
      ReadTTY -> maybe "" id <$> input
      WriteTTY msg -> output msg


echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo


-- Let's pretend
echoPure :: [String] -> Sem '[] ([String], ())
echoPure = flip runTeletypePure echo

pureOutput :: [String] -> [String]
pureOutput = fst . run . echoPure

-- echo forever
main :: IO ()
main = runM . teletypeToIO $ echo
```


Resource effect:

```haskell
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE GADTs, FlexibleContexts, TypeOperators, DataKinds, PolyKinds, TypeApplications #-}

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Resource

-- Using Teletype effect from above

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work $ \e -> writeTTY ("Caught " ++ show e)
  where work = bracket (readTTY) (const $ writeTTY "exiting bracket") $ \input -> do
          writeTTY "entering bracket"
          case input of
            "explode"     -> throw ThisException
            "weird stuff" -> writeTTY input >> throw ThatException
            _             -> writeTTY input >> writeTTY "no exceptions"

main :: IO (Either CustomException ())
main = (runM .@ lowerResource .@@ lowerError @CustomException) . teletypeToIO $ program
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
     . Sem (Resource ': r) a
    -> Sem r a
runResource = interpret $ \case
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
