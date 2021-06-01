{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, envname
}:
{ lib, pkgs, config, ... }:
with lib;

let
  cfg = config.services.intray."${envname}";
  concatAttrs = attrList: fold (x: y: x // y) { } attrList;
in
{
  options.services.intray."${envname}" =
    {
      enable = mkEnableOption "Intray Service";
      api-server = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Intray API Server";
            hosts = mkOption {
              type = types.listOf types.str;
              example = [ "api.intray.cs-syd.eu" ];
              default = [ ];
              description = "The host to serve API requests on";
            };
            port = mkOption {
              type = types.int;
              default = 8001;
              example = 8001;
              description = "The port to serve API requests on";
            };
            log-level = mkOption {
              type = types.str;
              default = "LevelWarn";
              example = "LevelInfo";
              description = "The log level";
            };
            admins = mkOption {
              type = types.nullOr (types.listOf types.str);
              default = null;
              example = [ "syd" ];
              description = "A list of the usernames that will have admin privileges";
            };
            freeloaders = mkOption {
              type = types.nullOr (types.listOf types.str);
              default = null;
              example = [ "syd" ];
              description = "A list of the usernames that will will be able to use the service for free";
            };
            monetisation = mkOption {
              default = null;
              type =
                types.nullOr (
                  types.submodule {
                    options = {
                      stripe-plan = mkOption {
                        type = types.str;
                        example = "plan_XXXXXXXXXXXXXX";
                        description = "Stripe plan for subscriptions.";
                      };
                      stripe-secret-key = mkOption {
                        type = types.str;
                        example = "sk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe secret key.";
                      };
                      stripe-publishable-key = mkOption {
                        type = types.str;
                        example = "pk_test_XXXXXXXXXXXXXXXXXXXXXXXX";
                        description = "Stripe publishable key.";
                      };
                    };
                  }
                );
            };
          };
        });
      };
      web-server = mkOption {
        default = null;
        type = types.nullOr (types.submodule {
          options = {
            enable = mkEnableOption "Intray Web Server";
            hosts = mkOption {
              type = types.listOf types.str;
              example = [ "intray.cs-syd.eu" ];
              default = [ ];
              description = "The host to serve web requests on";
            };
            port = mkOption {
              type = types.int;
              default = 8000;
              example = 8000;
              description = "The port to serve web requests on";
            };
            tracking-id = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "UA-53296133-1";
              description = "The tracking id for google analytics";
            };
            verification-tag = mkOption {
              type = types.nullOr types.str;
              default = null;
              example = "ADkAx2F-JQO9KJBBdLfAGuJ_OMqPOsX5MdGDsfd0Ggw";
              description = "The verification tag for google search console";
            };
          };
        });
      };
    };
  config =
    let
      intray-service =
        let
          workingDir = "/www/intray/${envname}/data/";
          intray-pkgs = pkgs.intrayPackages;
          configFile =
            let
              config =
                optionalAttrs (cfg.api-server.hosts != [ ])
                  {
                    api-host = head cfg.api-server.hosts;
                  } // {
                  api-port = cfg.api-server.port;
                  admins = cfg.api-server.admins;
                  freeloaders = cfg.api-server.freeloaders;
                  log-level = cfg.api-server.log-level;
                  monetisation =
                    optionalAttrs (!builtins.isNull cfg.api-server.monetisation) {
                      stripe-plan = cfg.monetisation.api-server.stripe-plan;
                      stripe-secret-key = cfg.monetisation.api-server.stripe-secret-key;
                      stripe-publishable-key = cfg.monetisation.api-server.stripe-publishable-key;
                    };
                  web-port = cfg.web-server.port;
                  tracking = cfg.web-server.tracking-id;
                  verification = cfg.web-server.verification-tag;
                };
            in
            pkgs.writeText "intray-config" (builtins.toJSON config);
        in
        {
          description = "Intray ${envname} Service";
          wantedBy = [ "multi-user.target" ];
          script =
            ''
              mkdir -p "${workingDir}"
              cd "${workingDir}"
              ${intray-pkgs.intray-web-server}/bin/intray-web-server serve  --config-file ${configFile}
            '';
          serviceConfig =
            {
              Restart = "always";
              RestartSec = 1;
              Nice = 15;
            };
          unitConfig =
            {
              # ensure Restart=always is always honoured
              StartLimitIntervalSec = 0;
            };

        };
      api-host = optionalAttrs (cfg.api-server.hosts != [ ]) {
        "${head (cfg.api-server.api-hosts)}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass =
              "http://localhost:${builtins.toString (cfg.api-server.port)}";
            serverAliases = tail cfg.api-server.hosts;
          };
      };
      web-host = optionalAttrs (cfg.web-server.hosts != [ ]) {
        "${head (cfg.web-server.web-hosts)}" =
          {
            enableACME = true;
            forceSSL = true;
            locations."/".proxyPass =
              "http://localhost:${builtins.toString (cfg.api-server.port)}";
            serverAliases = tail cfg.web-server.hosts;
          };
      };
    in
    mkIf cfg.enable {
      systemd.services =
        {
          "intray-${envname}" = intray-service;
        };
      networking.firewall.allowedTCPPorts = [ cfg.web-server.port cfg.api-server.port ];
      services.nginx.virtualHosts = api-host // web-host;
    };
}
