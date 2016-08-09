#!/bin/bash
set -eu
SCRIPT_DIR=$(dirname $(readlink -f $0))

source $SCRIPT_DIR/sling-config.sh

(
    flock -n 9 || exit 1

    cd $SCRIPT_DIR/server
    mkdir -p $SERVER_WORKDIR/.stack
    test -L ~/.stack || ln -s $SERVER_WORKDIR/.stack ~/.stack || echo "linking .stack failed, ignoring."
    stack setup
    stack build
    SLING_SERVER=$(stack path --project-root)/$(stack path --dist-dir)/build/sling-server-exe/sling-server-exe

    test -d $SERVER_WORKDIR/$GIT_CLONE_DIR_NAME || (cd $SERVER_WORKDIR && git clone $GIT_REPO $GIT_CLONE_DIR_NAME && git submodule update --init)
    cd $SERVER_WORKDIR/$GIT_CLONE_DIR_NAME
    bash -c "$SLING_SERVER poll $SERVER_OPTIONS -- $PREPUSH_COMMAND"

) 9>$SERVER_LOCKFILE
