#!/bin/sh
set +e

if [ -n "$1" ]; then
    exec 0<"$1"
fi

tail -c+2 | base64 -d | zlib-flate -uncompress
