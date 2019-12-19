import (
  let rev = "d85e435b7bded2596d7b201bcd938c94d8a921c1"; in
  fetchTarball "https://github.com/NixOS/nixpkgs-channels/archive/${rev}.tar.gz"
) {}
