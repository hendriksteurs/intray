let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;
in
pkgs.mkShell {
  name = "intray-nix-shell";
  buildInputs = pre-commit-hooks.tools;
  shellHook = ''
    ${pre-commit-hooks.run.shellHook}


    function nix-build_ {
      nix-build \
        --option extra-substituters https://validity.cachix.org \
        --option trusted-public-keys validity.cachix.org-1:CqZp6vt9ir3yB5f8GAtfkJxPZG8hKC5fhIdaQsf7eZE= \
        --option extra-substituters https://mergeless.cachix.org \
        --option trusted-public-keys mergeless.cachix.org-1:AFASeiTKF1aaIq8ZjqMF7xwPNKtKyEsUoU+8TgAwLsg= \
        --option extra-substituters https://intray.cachix.org \
        --option trusted-public-keys intray.cachix.org-1:qD7I/NQLia2iy6cbzZvFuvn09iuL4AkTmHvjxrQlccQ= \
        $*
    }
    alias nix-build=nix-build_
  '';
}
