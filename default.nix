# this is still a haskell callPackage-based derivation
# the names here can clash with non-haskell dependencies
# make sure to specify the names the explicitly that may be clashing
{ callPackage
, mkDerivation
, nix-gitignore
, makeWrapper
, hello
, lib
}:

let
  drv = (callPackage ./cabal.nix { inherit hello; });
in
  drv.overrideAttrs (attrs: {
    src = nix-gitignore.gitignoreSource [] attrs.src;
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ makeWrapper ];
    postFixup = ''
      wrapProgram $out/bin/haskell-demo-deps-exe \
        --set PATH ${lib.makeBinPath [
          hello
        ]}
    '';
  })
