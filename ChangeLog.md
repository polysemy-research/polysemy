# Changelog for polysemy

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

