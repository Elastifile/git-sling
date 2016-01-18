#!/bin/bash
set -eu
SCRIPT_DIR=$(dirname $(readlink -f $0))

source $SCRIPT_DIR/sling-config.sh

COMMAND="./tools/prepush.sh"

(
    flock -n 9 || exit 1

    cd $SCRIPT_DIR/server
    mkdir -p $SERVER_WORKDIR/.stack
    test -L ~/.stack || ln -s $SERVER_WORKDIR/.stack ~/.stack || echo "linking .stack failed, ignoring."
    stack setup
    stack build
    SLING_SERVER=$(stack path --project-root)/$(stack path --dist-dir)/build/sling-server-exe/sling-server-exe

    test -d $SERVER_WORKDIR/example-project-system || (cd $SERVER_WORKDIR && git clone git@github.com:Elastifile/example-project-system.git && git submodule update --init)
    cd $SERVER_WORKDIR/example-project-system/example-project
    "$SLING_SERVER" "$COMMAND"

) 9>$SERVER_LOCKFILE
