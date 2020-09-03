{ pkgs ? import ./pkgs.nix }:

with pkgs;
let
  haskellPackages = haskell.packages.ghc865;
  drv = haskellPackages.callPackage ./default.nix {
    hello = hello;
  };
in
  rec {
    library = drv;
    application = drv;
    docker = dockerTools.buildImage {
      name = application.name;
      contents = application;
      config = {
        Cmd = [ "/bin/haskell-demo-exe" ];
      };
    };
  }
