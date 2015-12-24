#!/bin/bash -eux
#
# USAGE: git-sling.sh "build_script.sh && test_foo"
#
# Reads proposed branches from origin, and tries to sling each of them
# onto staging, using attempt-branch.sh. Uses the order index (expects
# a format: sling/proposed/N/branch_name)
#
COMMAND="$1"
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
source $SCRIPT_DIR/sling-config.sh

git remote prune origin
git fetch
git checkout $STAGING
git reset --hard origin/$STAGING
git merge --ff-only origin/$MASTER
git push

git branch -r | \
    sed -e 's,^ *origin/\(.*\),\1,g' | \
    grep "^$PROPOSED_PREFIX.*" | \
    sort -g -t '/' | \
    xargs -r -n1  -t $SCRIPT_DIR/attempt-branch.sh $PROPOSED_PREFIX "$COMMAND"

