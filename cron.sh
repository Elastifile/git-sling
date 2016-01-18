#!/bin/bash
set -eu
SCRIPT_DIR=$(dirname $(readlink -f $0))

source $SOURCE_DIR/sling-config.sh

cd $SCRIPT_DIR/server
stack build
SLING_SERVER=$(stack path --project-root)/$(stack path --dist-dir)/build/sling-server-exe/sling-server-exe

test -d $SERVER_WORKDIR/example-project-system || (cd $SERVER_WORKDIR && git clone git@github.com:Elastifile/example-project-system.git && git submodule update --init)
cd $SERVER_WORKDIR/example-project-system/example-project
flock -n "$SERVER_LOCKFILE" "$SLING_SERVER" "$COMMAND"
