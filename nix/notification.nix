{ pkgs ? import ./pkgs.nix
}:
{ userName
, accessKey
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
  export INTRAY_PASSWORD="${accessKey}"
  ${pkgs.intrayPackages.intray-cli}/bin/intray login
  ${pkgs.intrayPackages.intray-cli}/bin/intray add --stdin --remote "$@"
  rm -rf $tempDir
''
