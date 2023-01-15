#!/bin/sh

set -eux

. .github/scripts/common.sh

git_describe

# ensure ghcup
install_ghcup

# install toolchain
ghcup -v install ghc --set --force "$GHC_VER"
ghcup -v install cabal --force "$CABAL_VER"
ghc --version
cabal --version
GHC="ghc-${GHC_VER}"

ecabal update

# test
ecabal sdist -z -o .
ecabal get bytestring-*.tar.gz
cd bytestring-*/
ecabal build -w "${GHC}" bytestring:tests --enable-tests --enable-benchmarks
ecabal test -w "${GHC}" --enable-tests --enable-benchmarks --test-show-details=direct all

# bench
ecabal bench -w "${GHC}" --enable-tests --enable-benchmarks --benchmark-option=-l all

# haddock
ecabal haddock -w "${GHC}" all

# check
ecabal check
