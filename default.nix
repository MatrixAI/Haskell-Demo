{ callPackage
, nix-gitignore
, makeWrapper
, hello
, lib
}:

let
  drv = (callPackage ./cabal.nix { hello = hello; });
in
  drv.overrideAttrs (attrs: {
    src = nix-gitignore.gitignoreSource [] ./.;
    nativeBuildInputs = attrs.nativeBuildInputs ++ [ makeWrapper ];
    postFixup = ''
      wrapProgram $out/bin/haskell-demo-deps-exe \
        --set PATH ${lib.makeBinPath [
          hello
        ]}
    '';
  })
