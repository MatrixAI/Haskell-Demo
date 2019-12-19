{ mkDerivation, base, hpack, stdenv }:
mkDerivation {
  pname = "haskell-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  preConfigure = "hpack";
  homepage = "https://github.com/MatrixAI/Haskell-Demo#readme";
  license = stdenv.lib.licenses.asl20;
}
