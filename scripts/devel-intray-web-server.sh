#!/usr/bin/env bash

set -ex


export PATH="$PATH:$(stack path --local-install-root)/bin"
export DEVELOPMENT=True

stack install :intray-server :intray-web-server \
  --file-watch --watch-all \
  --ghc-options='-freverse-errors -O0' \
  --fast \
  --no-nix-pure \
  --exec='./scripts/restart-intray-web-server.sh' \
  $@
