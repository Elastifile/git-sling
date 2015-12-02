#!/bin/bash -eu
# Proposes a new branch for slinging onto staging.
#
# USAGE:
#
#   git checkout <my_branch>
#   git-propose.sh
#
# This will create a new remote branch sling/proposed/N/my_branch,
# where N is the order in the queue
#
SLING_PREFIX="sling"
PROPOSED_PREFIX="$SLING_PREFIX/proposed/"
STAGING="staging"
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))

set -o pipefail

abort_unclean() {
    echo "Working dir is dirty! Use stash or clean."
    exit 1
}

abort_not_rebased() {
    echo "HEAD is not rebased over origin/$STAGING! Please rebase it before proposing."
    exit 1
}

git describe --dirty --all | grep -E ".*-dirty$" && abort_unclean
PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)

git fetch
git branch --merged HEAD -r | grep " *origin/$STAGING\$" || abort_not_rebased

echo "Proposing: $PROPOSED_BRANCH"

# The index here gives an approximate ordering (because it isn't
# atomic on the remove status). That's good enough for now.
INDEX=$(git branch -r | \
               (grep -E "$SLING_PREFIX.*/[0-9]+" \
                       || echo 0) | \
               ${SCRIPT_DIR}/sed.sh -r "s,.*$SLING_PREFIX.*/([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
NEXT_INDEX=$(($INDEX + 1))
git config user.email | grep "\-at\-" && \
    ( echo "your email contains '-at-' /";
      echo " we don't support    that!";
      exit 1)
EMAIL=$(git config user.email | ${SCRIPT_DIR}/sed.sh -s 's/@/-at-/g')
REMOTE_BRANCH="${PROPOSED_PREFIX}$NEXT_INDEX/$PROPOSED_BRANCH/$EMAIL"
git push origin "HEAD:$REMOTE_BRANCH"

echo "-----"
echo "Pushed to: $REMOTE_BRANCH"
echo "-----"
echo "To unpropose, use: git push --delete origin $REMOTE_BRANCH"
