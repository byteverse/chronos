{ mkDerivation, aeson, attoparsec, base, bytestring, hashable
, HUnit, primitive, QuickCheck, stdenv, test-framework
, test-framework-hunit, test-framework-quickcheck2, text, torsor
, vector
}:
mkDerivation {
  pname = "chronos";
  version = "1.0";
  src = ./.;
  libraryHaskellDepends = [
    aeson attoparsec base bytestring hashable primitive text torsor
    vector
  ];
  testHaskellDepends = [
    attoparsec base bytestring HUnit QuickCheck test-framework
    test-framework-hunit test-framework-quickcheck2 text
  ];
  homepage = "https://github.com/andrewthad/chronos#readme";
  description = "A performant time library";
  license = stdenv.lib.licenses.bsd3;
}
