# Changelog for polysemy

## 0.3.0.1 (2019-06-09)

- Fixed a type error in the benchmark caused by deprecation of `Semantic`

## 0.3.0.0 (2019-06-01)

- Removed all deprecated names
- Moved `Random` effect to `polysemy-zoo`
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

- Fixed a serious bug in `interpretH` and friends, where higher-order effects
    would always be run with the current interpreter.
- Lower precedence of `.@` and `.@@` to 8, from 9
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

