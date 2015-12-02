#!/bin/bash -eu

if [ `uname` == "Darwin" ]; then
    gxargs "$@"
else
    xargs "$@"
fi
