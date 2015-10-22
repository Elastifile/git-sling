#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="sling/$1"
TARGET_BRANCH_PREFIX="sling/$2"
COMMAND="$3"
SOURCE_DIR=$(dirname $BASH_SOURCE)

git remote prune origin
git fetch

git branch -r | \
    sed -e 's,^ *origin/\(.*\),\1,g' | \
    grep "^$SOURCE_BRANCH_PREFIX.*" | \
    xargs -r -n1  -t $SOURCE_DIR/attempt-branch.sh $SOURCE_BRANCH_PREFIX $TARGET_BRANCH_PREFIX "$COMMAND"

