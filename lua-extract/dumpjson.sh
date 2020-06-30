#!/bin/sh
set -e

here="$(dirname "$(realpath "$0")")"

LUA_PATH="$here/override/?.lua;$here/base/?.lua;;" lua dumpjson.lua | cabal run -v0
