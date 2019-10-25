{-# LANGUAGE CPP #-}

module DoctestSpec where

import Test.Hspec
import Test.DocTest

import Build_doctests (flags, pkgs, module_sources)

spec :: Spec
spec = parallel $ describe "Error messages" $ it "should pass the doctest" $ doctest $
  [ "--fast"
  , "-fobject-code"

#if __GLASGOW_HASKELL__ < 806
  , "-XMonadFailDesugaring"
  , "-XTypeInType"
#endif

  , "test/TypeErrors.hs"
  ]
  <> pkgs
  <> removeFlagSearchPathSrc flags

-- | Designed to remove flags that add the "polysemy-plugin/src" directory. For
-- example, it will remove the following flag:
-- "-i/Users/bob/code/polysemy/polysemy-plugin/src".
--
-- This was done because the presence of this flag causes the following error:
--   test/TypeErrors.hs:9: failure in expression `:set -fplugin=Polysemy.Plugin'
--   expected:   
--    but got: attempting to use module ‘main:Polysemy.Plugin’
--    (.../polysemy/polysemy-plugin/src/Polysemy/Plugin.hs) which is not loaded.
--
-- Without this flag, the tests pass as expected. My understanding of GHC isn't
-- great, so feel free to remove this function if you know of some way of making
-- this flag a non-issue.
removeFlagSearchPathSrc :: [String] -> [String]
removeFlagSearchPathSrc = filter (not . isSrcSearchPath)
  where
    isSrcSearchPath flag = isSearchPathFlag flag && isSrcDir flag

    isSearchPathFlag ('-':'i':_) = True
    isSearchPathFlag _           = False

    isSrcDir ('s':'r':c':[]) = True
    isSrcDir (x:xs)          = isSrcDir xs
    isSrcDir []              = False
