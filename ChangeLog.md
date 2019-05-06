# Changelog for polysemy

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

- Lowered precedence of `.@` and `.@@` down to 8, from 9
- Remove unnecessary dependency on `QuantifiedConstraints` (thanks to @jkachmar)
- Remove some `Typeable` constraints inside `Union` (thanks to @googleson78)
- Improved some haddock links

