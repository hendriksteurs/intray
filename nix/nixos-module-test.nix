{ sources ? import ./sources.nix
, pkgs ? import ./pkgs.nix { inherit sources; }
, intrayPackages ? pkgs.intrayPackages
}:
let
  intray-production = import (./nixos-module.nix) { inherit sources; inherit pkgs; envname = "production"; };
  home-manager = import (sources.home-manager + "/nixos/default.nix");

  api-port = 8001;
  web-port = 8002;
in
pkgs.nixosTest (
  { lib, pkgs, ... }: {
    name = "intray-module-test";
    nodes = {
      server = {
        imports = [
          intray-production
        ];
        services.intray.production = {
          enable = true;
          api-server = {
            enable = true;
            port = api-port;
            log-level = "LevelDebug";
          };
          web-server = {
            enable = true;
            port = web-port;
          };
        };
      };
      client = {
        imports = [
          home-manager
        ];
        users.users.testuser.isNormalUser = true;
        home-manager.users.testuser = { pkgs, ... }: {
          imports = [
            ./home-manager-module.nix
          ];
          home.stateVersion = "20.09";
          programs.intray = {
            enable = true;
            sync = {
              enable = true;
              url = "server:${builtins.toString api-port}";
              username = "testuser";
              password = "testpassword";
            };
          };
        };
      };
    };
    testScript = ''
      from shlex import quote

      server.start()
      client.start()
      server.wait_for_unit("multi-user.target")
      client.wait_for_unit("multi-user.target")

      server.wait_for_unit("intray-production.service")
      server.wait_for_open_port(${builtins.toString api-port})
      client.succeed("curl server:${builtins.toString api-port}")
      server.wait_for_open_port(${builtins.toString web-port})
      client.succeed("curl server:${builtins.toString web-port}")

      client.wait_for_unit("home-manager-testuser.service")


      def su(user, cmd):
          return f"su - {user} -c {quote(cmd)}"


      client.succeed(su("testuser", "cat ~/.config/intray/config.yaml"))

      # Something is wrong such that the register command hangs, for unknown reasons
      # machine.succeed(su("testuser", "intray register"))
      # machine.succeed(su("testuser", "intray login"))
      # machine.succeed(su("testuser", "intray sync"))
    '';
  }
)
