#!/usr/bin/env bash
set -x

cd $HOME

killall intray-server
killall intray-web-server

set -e

export INTRAY_SERVER_LOG_LEVEL="LevelDebug"
export INTRAY_WEB_SERVER_API_URL="localhost:8001"


intray-server serve \
  --admin admin &

intray-web-server serve &
