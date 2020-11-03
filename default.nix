let
  pkgs = import ./nix/pkgs.nix;
in
pkgs.intrayPackages // {
  inherit (pkgs) intrayNotification;
}
