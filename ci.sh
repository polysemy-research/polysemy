#!/usr/bin/env bash
set -xe

ghc --version
cabal --version

cabal update

cd polysemy
cabal build
cabal test
cabal haddock
cabal check
cabal sdist -o - 1>/dev/null

cd polysemy-plugin
cabal build    
cabal test    
cabal haddock
cabal check
cabal sdist -o - 1>/dev/null
