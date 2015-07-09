#!/bin/bash -ex

cabal install --only-dependencies --enable-tests
cabal configure --enable-tests
cabal test