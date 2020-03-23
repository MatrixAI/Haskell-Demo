{ mkDerivation, base, data-default-class, hpack, http-types
, iproute, monad-logger, mtl, network, safe-exceptions, stdenv
, text, transformers, wai, warp
}:
mkDerivation {
  pname = "haskell-demo";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers wai warp
  ];
  libraryToolDepends = [ hpack ];
  executableHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers wai warp
  ];
  testHaskellDepends = [
    base data-default-class http-types iproute monad-logger mtl network
    safe-exceptions text transformers wai warp
  ];
  prePatch = "hpack";
  homepage = "https://github.com/MatrixAI/Haskell-Demo#readme";
  license = stdenv.lib.licenses.asl20;
}
