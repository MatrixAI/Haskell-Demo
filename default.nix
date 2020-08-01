{ pkgs ? import ./pkgs.nix }:

with pkgs;
let
  drv = (haskellPackages.callPackage ./cabal.nix { inherit hello; });
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
