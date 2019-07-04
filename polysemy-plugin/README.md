# polysemy-plugin

[![Build Status](https://api.travis-ci.org/polysemy-research/polysemy.svg?branch=master)](https://travis-ci.org/isovector/polysemy-research)
[![Hackage](https://img.shields.io/hackage/v/polysemy-plugin.svg?logo=haskell)](https://hackage.haskell.org/package/polysemy-plugin)

## Dedication

> It doesn't matter how beautiful your theory is, it doesn't matter how smart
> you are. If it doesn't agree with experiment, it's wrong.
>
> Richard Feynman


## Overview

A typechecker plugin that can disambiguate "obvious" uses of effects in
[`polysemy`](https://hackage.haskell.org/package/polysemy).


## Example

Consider the following program:

```haskell
foo :: Member (State Int) r => Sem r ()
foo = put 10
```

What does this program do? Any human will tell you that it changes the state of
the `Int` to 10, which is clearly what's meant.

Unfortunately, `polysemy` can't work this out on its own. Its reasoning is
"maybe you wanted to change some other `State` effect which is *also* a `Num`,
but you just forgot to add a `Member` constraint for it."

This is obviously insane, but it's the way the cookie crumbles.
`polysemy-plugin` is a typechecker plugin which will disambiguate the above
program (and others) so the compiler will do what you want.


## Usage

Add the following line to your package configuration:

```
ghc-options: -fplugin=Polysemy.Plugin
```


## Limitations

The `polysemy-plugin` will only disambiguate effects if there is exactly one
relevant constraint in scope. For example, it will *not* disambiguate the
following program:

```haskell
bar :: Members '[ State Int
                , State Double
                ] r => Sem r ()
bar = put 10
```

because it is now unclear whether you're attempting to set the `Int` or the
`Double`. Instead, you can manually write a type application in this case.

```haskell
bar :: Members '[ State Int
                , State Double
                ] r => Sem r ()
bar = put @Int 10
```


## Acknowledgments

This plugin is copied almost verbatim from [`simple-effects`](https://hackage.haskell.org/package/simple-effects).

