{ mkDerivation, base, fetchgit, random, stdenv, tf-random }:
mkDerivation {
  pname = "savage";
  version = "1.0.0";
  src = fetchgit {
    url = "https://github.com/chessai/savage.git";
    sha256 = "1hcpydjx09i9iwfcpx9wwcj2j44yh62jgl6js14ibcgv9axyj24k";
    rev = "3b9d81a94631fa56ddd36ed564b18f14d9dfa4e4";
  };
  libraryHaskellDepends = [ base random tf-random ];
  homepage = "https://github.com/chessai/savage";
  description = "Re-exported random generators from QuickCheck";
  license = stdenv.lib.licenses.bsd3;
}
