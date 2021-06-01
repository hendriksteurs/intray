#!/usr/bin/env bash
set -x

cd $HOME

killall intray-server
killall intray-web-server

set -e

export INTRAY_SERVER_LOG_LEVEL=LevelDebug

intray-web-server serve \
  --admin admin &
