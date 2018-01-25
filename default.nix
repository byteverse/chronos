{ package ? "chronos", compiler ? "ghc822" }:
let
  fetchNixpkgs = import ./nix/fetchNixpkgs.nix;
  nixpkgs = fetchNixpkgs {
    rev = "01705125314fa0c7753f27c3dd7c4bfbda55c375"; 
    sha256 = "1a96vb4hlhnadm445lifq02wg2vz0a2hyxrcl6d0jy2cn7427aq6"; 
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
    with { cp = file: (self.callPackage (./nix/haskell + "/${file}.nix") {}); 
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
