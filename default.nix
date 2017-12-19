{ package ? "chronos", compiler ? "ghc822" }:

let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "597f819bc3dc1097a8daea632b51a7c065127b1f";
    sha256 = "1xzrgvhf0884s2ic88p7z14wzdp5sa454f028l5jw3290sd391bi";
  };
  pkgs = import nixpkgs { config = {}; };
  inherit (pkgs) haskell;

  filterPredicate = p: type:
    let path = baseNameOf p; in !(
       (type == "directory" && path == "dist")
    || (type == "symlink"   && path == "result")
    || (type == "directory" && path == ".git")
    || (type == "symlink"   && pkgs.lib.hasPrefix "result" path)
    || pkgs.lib.hasSuffix "~" path
    || pkgs.lib.hasSuffix ".o" path
    || pkgs.lib.hasSuffix ".so" path
    || pkgs.lib.hasSuffix ".nix" path);

  overrides = haskell.packages.${compiler}.override {
    overrides = self: super:
    with haskell.lib;
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}") {}); 
           build = name: path: self.callCabal2nix name (builtins.filterSource filterPredicate path) {}; 
         };

    {
      mkDerivation = args: super.mkDerivation (args // {
        doCheck = pkgs.lib.elem args.pname [ "chronos" ];
        doHaddock = false;
      });
      chronos = build "chronos" ./.;
    };
  };
in rec {
  drv = overrides.${package};
  chronos = if pkgs.lib.inNixShell then drv.env else drv;
}
