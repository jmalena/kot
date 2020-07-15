{ mkDerivation, base, bytestring, megaparsec, stdenv, tasty
, tasty-discover, tasty-hspec
}:
mkDerivation {
  pname = "lizzie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base bytestring megaparsec ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-discover tasty-hspec ];
  testToolDepends = [ tasty-discover ];
  license = stdenv.lib.licenses.bsd3;
}
