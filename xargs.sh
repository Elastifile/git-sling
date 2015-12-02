#!/bin/bash -eu

if [ `uname` == "Darwin" ]; then
    gxargs "$@"
else
    xarsg "$@"
fi
