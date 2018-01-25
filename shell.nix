{ package ? "chronos", compiler ? "ghc822" }:

(import ./default.nix {
  inherit package compiler;
}).chronos
