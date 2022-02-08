#!/usr/bin/env bash
set -ex

killall intray-server || true
killall intray-web-server || true

export INTRAY_SERVER_LOG_LEVEL="LevelDebug"
export INTRAY_WEB_SERVER_API_URL="localhost:8000"

cd intray-web-server

intray-server --admin admin &

intray-web-server &
