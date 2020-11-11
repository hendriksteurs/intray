let
  pkgs = import ./nix/pkgs.nix;
  pre-commit-hooks = import ./nix/pre-commit.nix;
  stripeCli =
    let
      src = builtins.fetchurl {
        url = "https://github.com/stripe/stripe-cli/releases/download/v1.5.3/stripe_1.5.3_linux_x86_64.tar.gz";
        sha256 = "sha256:0hbiv043h00n4xhzs1hbvgn7z1v91mb8djbd51qmbm12ab9qdr6c";
      };
    in
      pkgs.stdenv.mkDerivation {
        name = "stripe-cli";
        buildCommand = ''
          mkdir -p $out/bin
          tar xvzf ${src} --directory $out/bin
        '';
      };

in
pkgs.mkShell {
  name = "intray-nix-shell";
  buildInputs = pre-commit-hooks.tools ++ [ stripeCli ];
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
