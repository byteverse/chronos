#!/usr/bin/env bash

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist

#this is a workaround while cabal-repl is broken (https://github.com/haskell/cabal/issues/4602)
nix-shell -A chronos.env release.nix --run "runhaskell Setup.hs configure; runhaskell Setup.hs repl lib:chronos"

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist
