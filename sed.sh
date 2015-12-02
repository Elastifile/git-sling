#!/bin/bash -eu

if [ `uname` == "Darwin" ]; then
    gsed "$@"
else
    sed "$@"
fi
