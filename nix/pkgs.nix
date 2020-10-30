let
  pkgsv = import (import ./nixpkgs.nix);
  pkgs = pkgsv {};
  foobarPkgs =
    pkgsv {
      overlays =
        [
          (import ./gitignore-src.nix)
          (import ./overlay.nix)
        ];
      config.allowUnfree = true;
    };
in
foobarPkgs
