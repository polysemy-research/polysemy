# Changelog for polysemy

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

- **NEEDS MAJOR REVISION**: no longer require `inlineRecursiveCalls` --- the
    plugin does it automatically when compiling with `-O2`
- Deprecated `inlineRecursiveCalls`; slated for removal in the next version
- **NEEDS MAJOR PLUGIN REVISION**: plugin now automatically inlines recursive
    calls
- Lower precedence of `.@` and `.@@` to 8, from 9


