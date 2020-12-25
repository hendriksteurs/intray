let
  pkgsv = import (import ./nixpkgs.nix);
  validity-overlay =
    import (
      builtins.fetchGit (import ./validity-version.nix) + "/nix/overlay.nix"
    );
  sydtest-overlay =
    import (
      builtins.fetchGit (import ./sydtest-version.nix) + "/nix/overlay.nix"
    );

  pretty-relative-time-overlay =
    import (
      builtins.fetchGit (import ./pretty-relative-time-version.nix) + "/nix/overlay.nix"
    );
  mergeless-overlay =
    import (
      builtins.fetchGit (import ./mergeless-version.nix) + "/nix/overlay.nix"
    );
  yamlparse-applicative-overlay =
    import (
      builtins.fetchGit (import ./yamlparse-applicative-version.nix) + "/nix/overlay.nix"
    );

in
pkgsv {
  overlays =
    [
      validity-overlay
      pretty-relative-time-overlay
      mergeless-overlay
      yamlparse-applicative-overlay
      (import ./gitignore-src.nix)
      (import ./overlay.nix)
    ];
  config.allowUnfree = true;
}
