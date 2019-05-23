# Changelog for polysemy-plugin

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
