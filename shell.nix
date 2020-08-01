{ pkgs ? import ./pkgs.nix }:

with pkgs;
let
  haskellPackages = haskell.packages.ghc865;
  drv = (haskellPackages.callPackage ./default.nix {}).env;
in
  drv.overrideAttrs (attrs: {
    src = null;
    nativeBuildInputs = attrs.nativeBuildInputs ++ (with haskellPackages; [
      cabal-install
      cabal2nix
      hpack
    ]);
    shellHook = attrs.shellHook + ''
      echo 'Entering ${attrs.name}'
      set -v

      cabal2nix --hpack . >./cabal.nix
      hpack --force
      cabal v2-configure

      set +v
    '';
  })
