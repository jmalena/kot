{ mkDerivation, base, bytestring, containers, llvm-hs, llvm-hs-pure
, megaparsec, mtl, optparse-applicative, parser-combinators
, process, stdenv, string-qq, tasty, tasty-discover, tasty-hunit
, utf8-string
}:
mkDerivation {
  pname = "lizzie";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers llvm-hs llvm-hs-pure megaparsec mtl
    parser-combinators utf8-string
  ];
  executableHaskellDepends = [
    base bytestring optparse-applicative utf8-string
  ];
  testHaskellDepends = [
    base bytestring process string-qq tasty tasty-discover tasty-hunit
    utf8-string
  ];
  testToolDepends = [ tasty-discover ];
  license = stdenv.lib.licenses.bsd3;
}
