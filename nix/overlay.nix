final: previous:
with final.haskell.lib;

{
  intrayPackages =
    let
      pathFor = name: final.gitignoreSource (../. + "/${name}");
      intrayPkg =
        name:
          addBuildDepend (
            failOnAllWarnings (
              disableLibraryProfiling (final.haskellPackages.callCabal2nix name (pathFor name) {})
            )
          ) (final.haskellPackages.autoexporter);
      intrayPkgWithComp =
        exeName: name:
          generateOptparseApplicativeCompletion exeName (intrayPkg name);
      intrayPkgWithOwnComp = name: intrayPkgWithComp name name;
    in
      {

        "intray-api" = intrayPkg "intray-api";
        "intray-api-gen" = intrayPkg "intray-api-gen";
        "intray-cli" = intrayPkgWithComp "intray" "intray-cli";
        "intray-client" = intrayPkg "intray-client";
        "intray-data" = intrayPkg "intray-data";
        "intray-data-gen" = intrayPkg "intray-data-gen";
        "intray-server" = intrayPkgWithOwnComp "intray-server";
        "intray-server-gen" = intrayPkg "intray-server-gen";
        "intray-web-server" =
          let
            semantic-js =
              builtins.fetchurl {
                url =
                  https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.js;
                sha256 =
                  "sha256:0ll00jawcwd4nj568sj7lfp2ixrni9wqf37sz5nhz6wggjk9xhdp";
              };
            semantic-css =
              builtins.fetchurl {
                url =
                  https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/semantic.min.css;
                sha256 =
                  "sha256:0m13jdkv3vdqr0pbr1zfc2ndsafr2p5mnfzkbm7pd8v1ylwy8rpn";
              };
            jquery-js =
              builtins.fetchurl {
                url = https://code.jquery.com/jquery-3.1.1.min.js;
                sha256 =
                  "sha256:1gyrxy9219l11mn8c6538hnh3gr6idmimm7wv37183c0m1hnfmc5";
              };
            icons-ttf =
              builtins.fetchurl {
                url =
                  https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.ttf;
                sha256 =
                  "sha256:1nm34hrh3inyrq7cbkh47g8m2hbqpsgkzbdrpfiiii7m8bsq2zyb";
              };
            icons-woff =
              builtins.fetchurl {
                url =
                  https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff;
                sha256 =
                  "sha256:1qgzlmd80c4ckh9zpfl2qzjvg389hvmkdhkv8amyq4c71y2a9dlm";
              };
            icons-woff2 =
              builtins.fetchurl {
                url =
                  https://cdnjs.cloudflare.com/ajax/libs/semantic-ui/2.4.1/themes/default/assets/fonts/icons.woff2;
                sha256 =
                  "sha256:1lqd60f1pml8zc93hgwcm6amkcy6rnbq3cyxqv5a3a25jnsnci23";
              };
            intrayAndroidRelease =
              let
                repo =
                  final.fetchgit {
                    url =
                      "https://gitlab.com/Norfair/intray-android-release.git";
                    rev = "1df1b0d332f1f1326b4b29bc467c6f7671783b13";
                    sha256 =
                      "sha256:0jwf87w8j65vj1v0jxbn5mpa8pj3drwk31w1xy5fdbf9ddq10bgf";
                  };
              in
                repo + "/app-release.apk";
          in
            overrideCabal (intrayPkgWithOwnComp "intray-web-server") (
              old:
                {
                  preConfigure =
                    ''
                      ${old.preConfigure or ""}

                      mkdir -p static/
                      cp ${jquery-js} static/jquery.min.js
                      mkdir -p static/semantic/
                      cp ${semantic-css} static/semantic/semantic.min.css
                      cp ${semantic-js} static/semantic/semantic.min.js
                      mkdir -p static/semantic/themes/default/assets/fonts
                      cp ${icons-ttf} static/semantic/themes/default/assets/fonts/icons.ttf
                      cp ${icons-woff} static/semantic/themes/default/assets/fonts/icons.woff
                      cp ${icons-woff2} static/semantic/themes/default/assets/fonts/icons.woff2
                      cp ${intrayAndroidRelease} static/intray.apk
                    '';
                  postInstall =
                    let
                      linkcheck =
                        (
                          import (
                            builtins.fetchGit {
                              url = "https://github.com/NorfairKing/linkcheck";
                              rev = "dc65f22965d92e6145814cdc674d160f3c422559";
                              ref = "master";
                            }
                          )
                        ).linkcheck;
                      seocheck =
                        (
                          import (
                            builtins.fetchGit {
                              url = "https://github.com/NorfairKing/seocheck";
                              rev = "5a0314f103a2146ed5f3798e5a5821ab44e27c99";
                              ref = "master";
                            }
                          )
                        ).seocheck;
                    in
                      ''
                        $out/bin/intray-web-server serve &
                        sleep 0.5
                        ${linkcheck}/bin/linkcheck http://localhost:8000
                        ${seocheck}/bin/seocheck http://localhost:8000
                        ${final.killall}/bin/killall intray-web-server
                      '';
                }
            );
      };
  intrayNotification = import ./notification.nix { pkgs = final; };
  haskellPackages =
    previous.haskellPackages.override (
      old:
        {
          overrides =
            final.lib.composeExtensions (old.overrides or (_: _: {})) (
              self: super:
                let
                  typedUuidRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "typed-uuid";
                      rev = "dbc8fd4b56b78b1f9cf00bc2890d43dc19b97c5c";
                      sha256 =
                        "sha256:12b8na513xq5smlgwvqaqpplj8blfl452vdq0j2kv40qaw6y9qp7";
                    };
                  yesodStaticRemoteRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "yesod-static-remote";
                      rev = "22c0a92c1d62f1b8d432003844ef0636a9131b08";
                      sha256 =
                        "sha256:1mz1fb421wccx7mbpn9qaj214w4sl4qali5rclx9fqp685jkfj05";
                    };
                  looperRepo =
                    final.fetchFromGitHub {
                      owner = "NorfairKing";
                      repo = "looper";
                      rev = "8d6e69e99c5eb8f5f01b6bc36a2112962cb8d343";
                      sha256 =
                        "sha256:1sx5gc41vrmvcgrbh7g83zhrpqwz339g4fq0m1c15hhlz4480lh8";
                    };
                  servantAuthRepo =
                    final.fetchFromGitHub {
                      owner = "haskell-servant";
                      repo = "servant-auth";
                      rev = "23971e889f8cbe8790305bda8915f00aa8be5ad9";
                      sha256 =
                        "sha256:0q1n0s126ywqw3g9xiiaw59s9jn2543v7p4zgxw99p68pihdlysv";
                    };
                  persistentRepo =
                    final.fetchFromGitHub {
                      owner = "yesodweb";
                      repo = "persistent";
                      rev = "333be4996eb6eea2dc37d3a14858b668f0b9e381";
                      sha256 =
                        "sha256:1j76s7666vadm4q1ma73crkrks6q6nskzb3jqaf6rp2qmw1phfpr";
                    };
                  typedUuidPkg =
                    name:
                      self.callCabal2nix name (typedUuidRepo + "/${name}") {};
                  servantAuthPkg =
                    name:
                      doJailbreak (
                        self.callCabal2nix name (servantAuthRepo + "/${name}") {}
                      );
                  persistentPkg =
                    name:
                      overrideCabal (
                        # Because there is some nastiness that makes nix think we need the haskell sqlite library.
                        self.callCabal2nix name (persistentRepo + "/${name}") {}
                      ) (
                        old:
                          {
                            librarySystemDepends = [ final.sqlite ];
                          }
                      );
                in
                  {
                    yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" yesodStaticRemoteRepo {});
                    servant-auth-server = doJailbreak (super.servant-auth-server);
                    looper = self.callCabal2nix "looper" looperRepo {};

                  } // final.lib.genAttrs [
                    "typed-uuid"
                    "genvalidity-typed-uuid"
                  ] typedUuidPkg // final.lib.genAttrs [
                    "servant-auth"
                    "servant-auth-client"
                    "servant-auth-docs"
                    "servant-auth-swagger"
                    "servant-auth-server"
                  ] servantAuthPkg // final.lib.genAttrs [
                    "persistent"
                    "persistent-sqlite"
                    "persistent-template"
                  ] persistentPkg // final.intrayPackages
            );
        }
    );
}
