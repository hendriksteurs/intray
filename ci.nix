let
  sources = import ./nix/sources.nix;
  pkgs = import ./nix/pkgs.nix { inherit sources; };
  pre-commit-hooks = import ./nix/pre-commit.nix { inherit sources; };

in
{
  release = pkgs.intrayRelease;
  pre-commit-check = pre-commit-hooks.run;
  inherit (pkgs) intrayNotification;
  nixos-module-test = import ./nix/nixos-module-test.nix { inherit pkgs; };
}
