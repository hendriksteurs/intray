final: previous:
with final.lib;
with final.haskell.lib;

let
  sources = import ./sources.nix;


  generateOpenAPIClient = import (sources.openapi-code-generator + "/nix/generate-client.nix") { pkgs = final; };
  generatedStripe = generateOpenAPIClient {
    name = "stripe-client";
    configFile = ../stripe-client-gen.yaml;
    src = sources.stripe-spec + "/openapi/spec3.yaml";
  };
  generatedStripeCode = generatedStripe.code;

in
{
  intrayPackages =
    let
      intrayPkg = name:
        overrideCabal
          (
            final.haskellPackages.callCabal2nixWithOptions name
              (final.gitignoreSource (../. + "/${name}"))
              "--no-hpack"
              { }
          )
          (old: {
            doBenchmark = true;
            doHaddock = false;
            doCoverage = false;
            doHoogle = false;
            doCheck = false; # Only check the release version.
            hyperlinkSource = false;
            enableLibraryProfiling = false;
            enableExecutableProfiling = false;

            configureFlags = (old.configureFlags or [ ]) ++ [
              # Optimisations
              "--ghc-options=-O2"
              # Extra warnings
              "--ghc-options=-Wall"
              "--ghc-options=-Wincomplete-uni-patterns"
              "--ghc-options=-Wincomplete-record-updates"
              "--ghc-options=-Wpartial-fields"
              "--ghc-options=-Widentities"
              "--ghc-options=-Wredundant-constraints"
              "--ghc-options=-Wcpp-undef"
              "--ghc-options=-Werror"
            ];
            buildDepends = (old.buildDepends or [ ]) ++ [
              final.haskellPackages.autoexporter
            ];
            # Ugly hack because we can't just add flags to the 'test' invocation.
            # Show test output as we go, instead of all at once afterwards.
            testTarget = (old.testTarget or "") + " --show-details=direct";
          });
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
                linkcheck = (import sources.linkcheck).linkcheck;
                seocheck = (import sources.seocheck).seocheck;
              in
              ''
                ${old.postInstall or ""}

                export INTRAY_WEB_SERVER_API_URL=http://localhost:8000 # dummy

                ${final.intrayPackages.intray-server}/bin/intray-server --port 8000 &
                $out/bin/intray-web-server --port 8080 &

                sleep 0.5

                ${linkcheck}/bin/linkcheck http://localhost:8080
                ${seocheck}/bin/seocheck http://localhost:8080

                ${final.killall}/bin/killall intray-web-server
                ${final.killall}/bin/killall intray-server
              '';
          }
        );
    };

  intrayReleasePackages = mapAttrs
    (_: pkg: justStaticExecutables (doCheck pkg))
    final.intrayPackages;


  intrayRelease =
    final.symlinkJoin {
      name = "intray-release";
      paths = attrValues final.intrayReleasePackages;
    };

  intrayNotification = import ./notification.nix { pkgs = final; };

  inherit generatedStripeCode;

  haskellPackages =
    previous.haskellPackages.override (
      old:
      {
        overrides =
          composeExtensions (old.overrides or (_: _: { })) (
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
              in
              {
                yesod-static-remote = dontCheck (self.callCabal2nix "yesod-static-remote" yesodStaticRemoteRepo { });
                servant-auth-server = doJailbreak (super.servant-auth-server);
                envparse = self.callHackage "envparse" "0.4.1" { };
                yesod-autoreload = self.callCabal2nix "yesod-autoreload" sources.yesod-autoreload { };
                stripe-client = generatedStripe.package;
              } // genAttrs [
                "stripe-core"
                "stripe-haskell"
                "stripe-http-client"
                "stripe-http-streams"
              ]
                stripeHaskellPkg // genAttrs [
                "servant-auth"
                "servant-auth-client"
                "servant-auth-docs"
                "servant-auth-swagger"
                "servant-auth-server"
              ]
                servantAuthPkg // final.intrayPackages
          );
      }
    );
}
