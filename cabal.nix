{ mkDerivation, base, data-default-class, hello, hpack, http-types
, iproute, monad-logger, mtl, network, safe-exceptions, stdenv
, text, transformers, typed-process, wai, warp
}:
mkDerivation {
  pname = "haskell-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers typed-process wai warp
  ];
  libraryToolDepends = [ hello hpack ];
  executableHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers typed-process wai warp
  ];
  executableToolDepends = [ hello ];
  testHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers typed-process wai warp
  ];
  testToolDepends = [ hello ];
  prePatch = "hpack";
  homepage = "https://github.com/MatrixAI/Haskell-Demo#readme";
  license = stdenv.lib.licenses.asl20;
}
