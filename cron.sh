#!/bin/bash -eu
WORKDIR="$1"
COMMAND="$2"
LOCKFILE="$HOME/sling.lock"
SCRIPT_DIR=$(dirname $(readlink -f $0))

abort() {
   echo "Aborting."
   exit 1
}

[ -e $LOCKFILE ] && abort

trap "rm -f $LOCKFILE; exit 1" EXIT
touch $LOCKFILE
cd "$1"
$SCRIPT_DIR/git-sling.sh "$COMMAND"
