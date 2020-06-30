#!/bin/sh
set +e

if [ -n $1 ]; then
    exec 0<"$1"
fi

base64 -d | gunzip >&1
