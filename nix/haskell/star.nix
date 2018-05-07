{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "star";
  version = "0.0.0.2";
  src = ./.;
  libraryHaskellDepends = [ base ];
  homepage = "https://github.com/chessai/star#readme";
  description = "*-semirings";
  license = stdenv.lib.licenses.bsd3;
}
