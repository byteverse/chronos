#!/usr/bin/env bash

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist

nix-shell -A chronos  --run "cabal repl" 

# dist can cause problems with nix-build (called by nix-shell)
# so delete this in between builds
rm -rf dist
