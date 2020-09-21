let
  pkgs = import ./pkgs.nix;

in

  # Use an access key with the 'PermitSync' permission

  { userName
, accessKey ? null # If this is not set, the script will block to ask for a password.
, intrayUrl ? "https://api.intray.cs-syd.eu"
}:

    pkgs.writeShellScript "intray-notification" ''
  set -eou pipefail
  tempDir="$(mktemp -d intray-notification-XXXXXXXX)"
  export INTRAY_CACHE_DIR="$tempDir"
  export INTRAY_DATA_DIR="$tempDir"
  export INTRAY_CONFIG_FILE="$tempDir/config.yaml"
  export INTRAY_URL="${intrayUrl}"
  export INTRAY_USERNAME="${userName}"
  export INTRAY_SYNC_STRATEG9="NeverSync"
  ${pkgs.lib.optionalString (!builtins.isNull accessKey) ''
      export INTRAY_PASSWORD="${accessKey}"
  ''}
  ${pkgs.intrayPackages.intray-cli}/bin/intray login
  ${pkgs.intrayPackages.intray-cli}/bin/intray add --remote $*
  rm -rf $tempDir
''
