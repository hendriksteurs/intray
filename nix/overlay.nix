final: previous:
with final.haskell.lib;

let
  sources = import ./sources.nix;
in
{
  intrayPackages =
    let
      pathFor = name: final.gitignoreSource (../. + "/${name}");
      intrayPkg =
        name:
        addBuildDepend
          (
            failOnAllWarnings (
              disableLibraryProfiling (final.haskellPackages.callCabal2nixWithOptions name (pathFor name) "--no-hpack" { })
            )
          )
          final.haskellPackages.autoexporter;
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
          bulma-css =
            builtins.fetchurl {
              url = https://cdn.jsdelivr.net/npm/bulma@0.9.2/css/bulma.min.css;
              sha256 = "sha256:0nbwcsa1gi36f2aq9y96bap7glkp40k3g2bjb9s1vmg0011sri1v";
            };
          bulma-tooltip-css =
            builtins.fetchurl {
              url = https://cdn.jsdelivr.net/npm/bulma-tooltip@3.0.2/dist/css/bulma-tooltip.min.css;
              sha256 = "sha256:0xih9z80znhb3svn2xs6jbhh1mfkbywa1yjrq6p2llxk80md2yaw";
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
                ln -s ${jquery-js} static/jquery.min.js
                mkdir -p static/bulma/
                ln -s ${bulma-css} static/bulma/bulma.min.css
                ln -s ${bulma-tooltip-css} static/bulma/bulma-tooltip.min.css
                mkdir -p static/semantic/themes/default/assets/fonts
                ln -s ${icons-ttf} static/semantic/themes/default/assets/fonts/icons.ttf
                ln -s ${icons-woff} static/semantic/themes/default/assets/fonts/icons.woff
                ln -s ${icons-woff2} static/semantic/themes/default/assets/fonts/icons.woff2
                ln -s ${intrayAndroidRelease} static/intray.apk
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
                export INTRAY_WEB_SERVER_API_URL=http://localhost:8001 # dummy
                ${final.intrayPackages.intray-server}/bin/intray-server serve &
                $out/bin/intray-web-server serve &
                sleep 0.5
                ${linkcheck}/bin/linkcheck http://localhost:8000
                ${seocheck}/bin/seocheck http://localhost:8000
                ${final.killall}/bin/killall intray-web-server
              '';
          }
        );
    };
  intrayRelease =
    final.symlinkJoin {
      name = "intray-release";
      paths = final.lib.attrValues final.intrayPackages;
    };

  intrayNotification = import ./notification.nix { pkgs = final; };
  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          final.lib.composeExtensions (old.overrides or (_: _: { })) (
            self: super:
              let
                stripeHaskellRepo =
                  final.fetchFromGitHub {
                    owner = "NorfairKing";
                    repo = "stripe";
                    rev = "008e992cae9c9bdb025bcf575c1bdf1037632a8a";
                    sha256 =
                      "sha256:1sxp8phdw1ahndy6h9q4ad0hdfraxyy5qnjd7w80v6m83py419gk";
                  };
                yesodStaticRemoteRepo =
                  final.fetchFromGitHub {
                    owner = "NorfairKing";
                    repo = "yesod-static-remote";
                    rev = "22c0a92c1d62f1b8d432003844ef0636a9131b08";
                    sha256 =
                      "sha256:1mz1fb421wccx7mbpn9qaj214w4sl4qali5rclx9fqp685jkfj05";
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
                stripeHaskellPkg =
                  name:
                  dontCheck (
                    self.callCabal2nix name (stripeHaskellRepo + "/${name}") { }
                  );
                servantAuthPkg =
                  name:
                  doJailbreak (
                    self.callCabal2nix name (servantAuthRepo + "/${name}") { }
                  );
                persistentPkg =
                  name:
                  overrideCabal
                    (
                      # Because there is some nastiness that makes nix think we need the haskell sqlite library.
                      self.callCabal2nix name (persistentRepo + "/${name}") { }
                    )
                    (
                      old:
                      {
                        librarySystemDepends = [ final.sqlite ];
                      }
                    );
              in
              {
                yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" yesodStaticRemoteRepo { });
                servant-auth-server = doJailbreak (super.servant-auth-server);
                looper = self.callCabal2nix "looper" (sources.looper + "/looper") { };
                envparse = self.callHackage "envparse" "0.4.1" { };
              } // final.lib.genAttrs [
                "stripe-core"
                "stripe-haskell"
                "stripe-http-client"
                "stripe-http-streams"
              ]
                stripeHaskellPkg // final.lib.genAttrs [
                "servant-auth"
                "servant-auth-client"
                "servant-auth-docs"
                "servant-auth-swagger"
                "servant-auth-server"
              ]
                servantAuthPkg // final.lib.genAttrs [
                "persistent"
                "persistent-sqlite"
                "persistent-template"
              ]
                persistentPkg // final.intrayPackages
          );
      }
    );
}
