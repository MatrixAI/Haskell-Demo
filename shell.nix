{
  pkgs ? import (fetchTarball https://github.com/NixOS/nixpkgs-channels/archive/8b1cf100cd8badad6e1b6d4650b904b88aa870db.tar.gz) {}
}:
  with pkgs;
  haskell.lib.buildStackProject {
    name = "graphql-demo";
    src = null;
    buildInputs = [];
    shellHook = ''
      echo 'Entering GraphQL Demo Environment'
      set -v

      alias stack='\stack --nix'

      set +v
    '';
  }
