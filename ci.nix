let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;

in
pkgs.intrayPackages // {
  inherit (pkgs) intrayNotification;
  pre-commit-check = pre-commit-hooks.run;
}
