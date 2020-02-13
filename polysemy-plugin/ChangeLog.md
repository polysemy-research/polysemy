# Changelog for polysemy-plugin


## 0.2.5.0 (2020-10-14)
- Updated the lower bounds to `polysemy-1.3.0.0` because of changes to
    `polysemy` internals
- Updated the test suite to test against `polysemy-1.3.0.0`.

## 0.2.4.0 (2019-10-29)

- The plugin now works on GHC 8.8.1 (thanks to @googleson78 and @sevanspowell)
- Improved error messages for when you forgot to include `polysemy` itself

## 0.2.3.0 (2019-09-04)

- The plugin will now choose between given effects based on the ability to unify them.
    This makes it possible for disambiguation to kick in even when using multiple
    instances of the same effect with different type variables,
    as long as type annotations/applications are used to
    target a specific instance.
- Updated the test suite to test against `polysemy-1.2.0.0`.

## 0.2.2.0 (2019-07-04)

- The plugin will now prevent some false-positives in `polysemy`'s error
    messages
- Updated the lower bounds to `polysemy-0.6.0.0`

## 0.2.1.1 (2019-06-26)

- Updated the test suite to test against `polysemy-0.5.0.0`

## 0.2.1.0 (2019-06-14)

- Greatly improved the plugin's ability to unify polymorphic types when running
    interpreters.

## 0.2.0.3 (2019-06-13)

- Fixed a bug where the plugin could (incorrectly) loop indefinitely attempting
    to solve some constraints.
- Changed the lower-bound of `inspection-testing` to allow Cabal users to
    successfully run the test-suite.

## 0.2.0.2 (2019-06-09)

- Fixed a bug where the plugin wouldn't attempt to unify effects recursively
- Updated the test suite to test against `polysemy-0.3`

## 0.2.0.1 (2019-05-28)

- Fixed a bug where the plugin would get confused in the context of legitimate
    type errors

## 0.2.0.0 (2019-05-23)

- Fixed a serious bug where the changes from 0.1.0.1 broke most real-world
    usages of polysemy
- The plugin will now automatically perform the transformation in
    `polysemy`'s `inlineRecursiveCalls` when run with `-O`

## 0.1.0.1 (2019-05-18)

- Added some explicit bounds for cabal
- Fixed a bug where effects that were too polymorphic would silently be accepted

## 0.1.0.0 (2019-04-27)

- Initial release

## Unreleased changes

- Added `mapError`

