#!/bin/bash -eux
#
# USAGE: git-sling.sh proposed/ "build_script.sh && test_foo"
#
# Reads proposed branches from origin, and tries to sling each of them
# onto staging, using attempt-branch.sh. Uses the order index (expects
# a format: sling/proposed/N/branch_name)
#
SOURCE_BRANCH_PREFIX="sling/$1"
COMMAND="$2"
SOURCE_DIR=$(dirname $BASH_SOURCE)

git remote prune origin
git fetch

git branch -r | \
    sed -e 's,^ *origin/\(.*\),\1,g' | \
    grep "^$SOURCE_BRANCH_PREFIX.*" | \
    sort -g -t '/' | \
    xargs -r -n1  -t $SOURCE_DIR/attempt-branch.sh $SOURCE_BRANCH_PREFIX "$COMMAND"

