<p align="center">
<img src="https://raw.githubusercontent.com/polysemy-research/polysemy/master/polysemy.png" alt="Polysemy" title="Polysemy">
</p>

<p>&nbsp;</p>

# polysemy

[![Build Status](https://api.travis-ci.org/polysemy-research/polysemy.svg?branch=master)](https://travis-ci.org/polysemy-research/polysemy)
[![Hackage](https://img.shields.io/hackage/v/polysemy.svg?logo=haskell&label=polysemy)](https://hackage.haskell.org/package/polysemy)
[![Hackage](https://img.shields.io/hackage/v/polysemy-plugin.svg?logo=haskell&label=polysemy-plugin)](https://hackage.haskell.org/package/polysemy-plugin)
[![Zulip chat](https://img.shields.io/badge/zulip-join_chat-brightgreen.svg)](https://funprog.zulipchat.com/#narrow/stream/216942-Polysemy)

## Overview

`polysemy` is a library for writing high-power, low-boilerplate domain specific
languages. It allows you to separate your business logic from your
implementation details. And in doing so, `polysemy` lets you turn your
implementation code into reusable library code.

It's like `mtl` but composes better, requires less boilerplate, and avoids the
O(n^2) instances problem.

It's like `freer-simple` but more powerful.

It's like `fused-effects` but with an order of magnitude less boilerplate.

Additionally, unlike `mtl`, `polysemy` has no functional dependencies, so you
can use multiple copies of the same effect. This alleviates the need for ~~ugly
hacks~~ band-aids like
[classy lenses](http://hackage.haskell.org/package/lens-4.17.1/docs/Control-Lens-TH.html#v:makeClassy),
the
[`ReaderT` pattern](https://www.fpcomplete.com/blog/2017/06/readert-design-pattern)
and nicely solves the
[trouble with typed errors](https://www.parsonsmatt.org/2018/11/03/trouble_with_typed_errors.html).

Concerned about type inference? `polysemy` comes with its companion
[`polysemy-plugin`](https://github.com/isovector/polysemy/tree/master/polysemy-plugin),
which helps it perform just as well as `mtl`'s! Add `polysemy-plugin` to your
`package.yaml` or `.cabal` file's `dependencies` section to use. Then turn it on with a pragma in your source files:

```haskell
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
```

Or by adding `-fplugin=Polysemy.Plugin` to your `package.yaml`/`.cabal` file `ghc-options` section.

## Features

- *Effects are higher-order,* meaning it's trivial to write `bracket` and `local`
    as first-class effects.
- *Effects are low-boilerplate,* meaning you can create new effects in a
    single-digit number of lines. New interpreters are nothing but functions and
    pattern matching.

## Tutorials and Resources

- Raghu Kaippully wrote a beginner friendly
  [tutorial](https://haskell-explained.gitlab.io/blog/posts/2019/07/28/polysemy-is-cool-part-1/index.html).
- Sandy Maguire, the author, wrote a post about
  [Porting to Polysemy](https://reasonablypolymorphic.com/blog/porting-to-polysemy/)
  from transformers/MTL-style monads.
- Paweł Szulc gave a [great talk](https://youtu.be/idU7GdlfP9Q?t=1394) on how
  to start thinking about polysemy.
- Sandy Maguire gave a talk on some of the
  [performance implementation](https://www.youtube.com/watch?v=-dHFOjcK6pA)
- He has also written
  [some](http://reasonablypolymorphic.com/blog/freer-higher-order-effects/)
  [blog posts](http://reasonablypolymorphic.com/blog/tactics/) on other
  implementation details.

## Examples

Make sure you read the
[Necessary Language Extensions](https://github.com/polysemy-research/polysemy#necessary-language-extensions)
before trying these yourself!

Teletype effect:

```haskell
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}

import Polysemy
import Polysemy.Input
import Polysemy.Output

data Teletype m a where
  ReadTTY  :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

runTeletypePure :: [String] -> Sem (Teletype ': r) a -> Sem r ([String], a)
runTeletypePure i
  -- For each WriteTTY in our program, consume an output by appending it to the
  -- list in a ([String], a)
  = runOutputMonoid pure
  -- Treat each element of our list of strings as a line of input
  . runInputList i
  -- Reinterpret our effect in terms of Input and Output
  . reinterpret2 \case
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
{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications #-}

import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Error
import Polysemy.Resource

-- Using Teletype effect from above

data CustomException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomException] r => Sem r ()
program = catch @CustomException work \e -> writeTTY $ "Caught " ++ show e
 where
  work = bracket (readTTY) (const $ writeTTY "exiting bracket") \input -> do
    writeTTY "entering bracket"
    case input of
      "explode"     -> throw ThisException
      "weird stuff" -> writeTTY input *> throw ThatException
      _             -> writeTTY input *> writeTTY "no exceptions"

main :: IO (Either CustomException ())
main
  = runFinal
  . embedToFinal @IO
  . resourceToIOFinal
  . errorToIOFinal @CustomException
  . teletypeToIO
  $ program
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

```txt
• 'Resource' is higher-order, but 'interpret' can help only
  with first-order effects.
  Fix:
    use 'interpretH' instead.
• In the expression:
    interpret
      $ \case
```

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

## Building with Nix

The project provides a basic nix config for building in development.
It is defined as a [flake] with backwards compatibility stubs in `default.nix` and `shell.nix`.

To build the main library or plugin:

```bash
nix-build -A polysemy
nix-build -A polysemy-plugin
```

Flake version:

```bash
nix build
nix build '.#polysemy-plugin'
```

To inspect a dependency:

```bash
nix repl

> p = import ./.
> p.unagi-chan
```

To run a shell command with all dependencies in the environment:

```bash
nix-shell --pure
nix-shell --pure --run 'cabal v2-haddock polysemy'
nix-shell --pure --run ghcid
```

Flake version:

```bash
nix develop -i # just enter a shell
nix develop -i -c cabal v2-haddock polysemy
nix develop -i -c haskell-language-server-wrapper # start HLS for your IDE
```

## *What about performance?*

Previous versions of this `README` mentioned **the library being**
***zero-cost***, as in having no visible effect on performance. While this was
the original motivation and main factor in implementation of this library, it
turned out that
[**optimizations** we depend on](https://reasonablypolymorphic.com/blog/specialization/),
while showing amazing results in small benchmarks, **don't work in
[bigger, multi-module programs](https://github.com/ghc-proposals/ghc-proposals/pull/313#issuecomment-590143835)**,
what greatly limits their usefulness.

What's more interesting though is that
this **isn't a `polysemy`-specific** problem - basically **all popular effects
libraries** ended up being bitten by variation of this problem in one way or
another, resulting in
[visible drop in performance](https://github.com/lexi-lambda/ghc-proposals/blob/delimited-continuation-primops/proposals/0000-delimited-continuation-primops.md#putting-numbers-to-the-cost)
compared to equivalent code without use of effect systems.

*Why did nobody notice this?*

One factor may be that while GHC's optimizer is
very, very good in general in optimizing all sorts of abstraction, it's
relatively complex and hard to predict - authors of libraries may have not
deemed location of code relevant, even though it had big effect at the end.
The other is that maybe **it doesn't matter as much** as we like to tell
ourselves. Many of these effects
libraries are used in production and they're doing just fine, because maximum
performance usually matters in small, controlled areas of code, that often
don't use features of effect systems at all.

## Acknowledgements, citations, and related work

The following is a non-exhaustive list of people and works that have had a
significant impact, directly or indirectly, on `polysemy`’s design and
implementation:

- Oleg Kiselyov, Amr Sabry, and Cameron Swords —
  [Extensible Effects: An alternative to monad transfomers][oleg:exteff]
- Oleg Kiselyov and Hiromi Ishii —
  [Freer Monads, More Extensible Effects][oleg:more]
- Nicolas Wu, Tom Schrijvers, and Ralf Hinze —
  [Effect Handlers in Scope][wu:scope]
- Nicolas Wu and Tom Schrijvers —
  [Fusion for Free: Efficient Algebraic Effect Handlers][schrijvers:fusion]
- Andy Gill and other contributors — [`mtl`][hackage:mtl]
- Rob Rix, Patrick Thomson, and other contributors —
  [`fused-effects`][gh:fused-effects]
- Alexis King and other contributors — [`freer-simple`][gh:freer-simple]

[docs]: https://hasura.github.io/eff/Control-Effect.html
[gh:fused-effects]: https://github.com/fused-effects/fused-effects
[gh:freer-simple]: https://github.com/lexi-lambda/freer-simple
[hackage:mtl]: https://hackage.haskell.org/package/mtl
[oleg:exteff]: http://okmij.org/ftp/Haskell/extensible/exteff.pdf
[oleg:more]: http://okmij.org/ftp/Haskell/extensible/more.pdf
[schrijvers:fusion]: https://people.cs.kuleuven.be/~tom.schrijvers/Research/papers/mpc2015.pdf
[wu:scope]: https://www.cs.ox.ac.uk/people/nicolas.wu/papers/Scope.pdf
[flake]: https://wiki.nixos.org/wiki/Flakes
