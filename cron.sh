#!/bin/bash -eu
WORKDIR="$1"
COMMAND="$2"
LOCKFILE="$HOME/sling.lock"

abort() {
   echo "Aborting."
   exit 1
}

[ -e $LOCKFILE ] && abort

trap "rm -f $LOCKFILE; exit 1" EXIT
touch $LOCKFILE
cd "$1"
~/repos/git-sling/git-sling.sh "$COMMAND"

