#!/bin/bash -eu
WORKDIR="$1"
COMMAND="$2"
LOCKFILE="/tmp/sling.lock"
SCRIPT_DIR=$(dirname $(readlink -f $0))

abort() {
   echo "Aborting."
   exit 1
}

[ -e $LOCKFILE ] && abort

trap "rm -f $LOCKFILE; exit 1" EXIT
touch $LOCKFILE
# the script didn't fail on this cd failing!!!!
cd "$WORKDIR"
$SCRIPT_DIR/git-sling.sh "$COMMAND"
