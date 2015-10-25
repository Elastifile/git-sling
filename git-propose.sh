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

git describe --dirty --all | (grep -E ".*-dirty$" && abort_unclean || echo "Working directory clean")
PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)

echo "Proposing: $PROPOSED_BRANCH"

git fetch

# The index here gives an approximate ordering (because it isn't
# atomic on the remove status). That's good enough for now.
INDEX=$(git branch -r | \
               (grep -E "$SLING_PREFIX.*/[0-9]+" \
                       || echo 0) | \
               sed -r "s,.*$SLING_PREFIX.*/([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
NEXT_INDEX=$(($INDEX + 1))
REMOTE_BRANCH="${PROPOSED_PREFIX}$NEXT_INDEX/$PROPOSED_BRANCH"
git push origin "$PROPOSED_BRANCH:$REMOTE_BRANCH"

echo "-----"
echo "Pushed to: $REMOTE_BRANCH"
echo "-----"
echo "To unpropose, use: git push --delete origin $REMOTE_BRANCH"
