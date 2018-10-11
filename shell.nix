{
  pkgs ? import ./pkgs.nix,
  haskellPath ? "ghc843"
}:
  with pkgs;
  let
    haskellPackages = lib.getAttrFromPath (lib.splitString "." haskellPath) haskell.packages;
    drv = (import ./default.nix { inherit pkgs haskellPath; }).env;
  in
    drv.overrideAttrs (attrs: {
      src = null;
      buildInputs = attrs.buildInputs ++ (with haskellPackages; [
        cabal2nix
        hpack
        cabal-install
      ]);
      shellHook = attrs.shellHook + ''
        echo 'Entering ${attrs.name}'
        set -v

        cabal2nix --hpack . >./cabal.nix
        hpack
        cabal configure

        set +v
      '';
    })
