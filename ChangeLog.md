# Changelog for polysemy

## 1.0.0.0 (2019-07-24)

### Breaking Changes

- Renamed `Lift`  to `Embed` (thanks to @googleson78)
- Renamed `runAsyncInIO` to `lowerAsync`
- Renamed `runAsync` to `asyncToIO`
- Renamed `runBatchOutput` to `runOutputBatched`
- Renamed `runConstInput` to `runInputConst`
- Renamed `runEmbed` to `runEmbedded` (thanks to @googleson78)
- Renamed `runEmbedded` to `lowerEmbedded`
- Renamed `runErrorAsAnother` to `mapError`
- Renamed `runErrorInIO` to `lowerError`
- Renamed `runFoldMapOutput` to `runOutputMonoid`
- Renamed `runIO` to `embedToMonadIO`
- Renamed `runIgnoringOutput` to `ignoreOutput`
- Renamed `runIgnoringTrace` to `ignoreTrace`
- Renamed `runInputAsReader` to `inputToReader`
- Renamed `runListInput` to `runInputList`
- Renamed `runMonadicInput` to `runInputSem`
- Renamed `runOutputAsList` to `runOutputList`
- Renamed `runOutputAsTrace` to `outputToTrace`
- Renamed `runOutputAsWriter` to `outputToWriter`
- Renamed `runResourceBase` to `resourceToIO`
- Renamed `runResourceInIO` to `lowerResource`
- Renamed `runStateInIORef` to `runStateIORef`
- Renamed `runTraceAsList` to `runTraceList`
- Renamed `runTraceAsOutput` to `traceToOutput`
- Renamed `runTraceIO` to `traceToIO`
- Renamed `sendM` to `embed` (thanks to @googleson78)
- The `NonDet` effect is now higher-order (thanks to @KingoftheHomeless)

### Other Changes

- Added `evalState` and `evalLazyState`
- Added `runNonDetMaybe` (thanks to @KingoftheHomeless)
- Added `nonDetToError` (thanks to @KingoftheHomeless)
- Haddock documentation for smart constructors generated via `makeSem` will no
    longer have weird variable names (thanks to @TheMatten)


## 0.7.0.0 (2019-07-08)

### Breaking Changes

- Added a `Pass` constructor to `Writer` (thanks to @KingoftheHomeless)
- Fixed a bug in `runWriter` where the MTL semantics wouldn't be respected (thanks to @KingoftheHomeless)
- Removed the `Censor` constructor of `Writer` (thanks to @KingoftheHomeless)
- Renamed `Yo` to `Weaving`
- Changed the visible type applications for `asks`, `gets`, and `runErrorAsAnother`

### Other Changes

- Fixed haddock generation

## 0.6.0.0 (2019-07-04)

### Breaking Changes

- Changed the type of `runBatchOutput` to be more useful (thanks to @Infinisil)

### Other Changes

- **THE ERROR MESSAGES ARE SO MUCH BETTER** :party: :party: :party:
- Added `runEmbedded` to `Polysemy.IO`
- Added `runOutputAsList` to `Polysemy.Output` (thanks to @googleson78)
- Asymptotically improved the performance of `runTraceAsList` (thanks to
    @googleson78)

## 0.5.1.0 (2019-06-28)

- New combinators for `Polysemy.Error`: `fromEither` and `fromEitherM`

## 0.5.0.1 (2019-06-27)

- Fixed a bug where `intercept` and `interceptH` wouldn't correctly handle
    higher-order effects

## 0.5.0.0 (2019-06-26)

### Breaking Changes

- Removed the internal `Effect` machinery

### New Effects and Interpretations

- New effect; `Async`, for describing asynchronous computations
- New interpretation for `Resource`: `runResourceBase`, which can lower
    `Resource` effects without giving a lowering natural transformation
- New interpretation for `Trace`: `runTraceAsList`
- New combinator: `withLowerToIO`, which is capable of transforming
    `IO`-invariant functions as effects.

### Other Changes

- Lots of hard work on the package and CI infrastructure to make it green on
    GHC 8.4.4 (thanks to @jkachmar)
- Changed the order of the types for `runMonadicInput` to be more helpful
    (thanks to @tempname11)
- Improved the error machinery to be more selective about when it runs
- Factored out the TH into a common library for third-party consumers

## 0.4.0.0 (2019-06-12)

### Breaking Changes

- Renamed `runResource` to `runResourceInIO`

### Other Changes

- Added `runResource`, which runs a `Resource` purely
- Added `onException`, `finally` and `bracketOnError` to `Resource`
- Added a new function, `runResource` which performs bracketing for pure code

## 0.3.0.1 (2019-06-09)

- Fixed a type error in the benchmark caused by deprecation of `Semantic`

## 0.3.0.0 (2019-06-01)

### Breaking Changes

- Removed all deprecated names
- Moved `Random` effect to `polysemy-zoo`

### Other Changes

- `makeSem` can now be used to create term-level operators (thanks to
    @TheMatten)

## 0.2.2.0 (2019-05-30)

- Added `getInspectorT` to the `Tactical` functions, which allows polysemy code
    to be run in external callbacks
- A complete rewrite of `Polysemy.Internal.TH.Effect` (thanks to @TheMatten)
- Fixed a bug in the TH generation of effects where the splices could contain
    usages of effects that were ambiguous

## 0.2.1.0 (2019-05-27)

- Fixed a bug in the `Alternative` instance for `Sem`, where it would choose the
    *last* success instead of the first
- Added `MonadPlus` and `MonadFail` instances for `Sem`

## 0.2.0.0 (2019-05-23)

### Breaking Changes

- Lower precedence of `.@` and `.@@` to 8, from 9

### Other Changes

- Fixed a serious bug in `interpretH` and friends, where higher-order effects
    would always be run with the current interpreter.
- Users need no longer require `inlineRecursiveCalls` --- the
    `polysemy-plugin-0.2.0.0` will do  it automatically when compiling with `-O`
- Deprecated `inlineRecursiveCalls`; slated for removal in the next version

## 0.1.2.1 (2019-05-18)

- Give explicit package bounds for dependencies
- Haddock improvements
- Remove `Typeable` machinery from `Polysemy.Internal.Union` (thanks to
    @googleson78)

## 0.1.2.0 (2019-04-26)

- `runInputAsReader`, `runTraceAsOutput` and `runOutputAsWriter` have more
    generalized types
- Added `runStateInIO`
- Added `runOutputAsTrace`
- Added `Members` (thanks to @TheMatten)


## 0.1.1.0 (2019-04-14)

- Added `runIO` interpretation (thanks to @adamConnerSax)
- Minor documentation fixes


## 0.1.0.0 (2019-04-11)

- Initial release

## Unreleased changes

- Changed the tyvars of `fromEitherM`, `runErrorAsAnother`, `runEmbedded`,
  `asks` and `gets`

