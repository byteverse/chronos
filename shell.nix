{ package ? "chronos", compiler ? "ghc841" }:

(import ./default.nix {
  inherit package compiler;
}).chronos
