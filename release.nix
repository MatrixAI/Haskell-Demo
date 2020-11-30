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
    # justStaticExecutables applies static linking to haskell libraries. 
    # System libraries are still dynamically linked.
    applicationStatic = pkgs.haskell.lib.justStaticExecutables drv;
    docker = dockerTools.buildImage {
      name = application.name;
      contents = application;
      config = {
        Cmd = [ "/bin/haskell-demo-library-exe" ];
      };
    };
  }
