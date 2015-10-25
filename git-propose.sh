#!/bin/bash -eux
# Proposes a new branch for slinging onto staging.
#
# USAGE: git-propose.sh my_branch
#
# This will create a new remote branch sling/proposed/N/my_branch,
# where N is the order in the queue
#
PROPOSED_BRANCH="$1"
PROPOSED_PREFIX="sling/proposed/"

git fetch

# The index here gives an approximate ordering (because it isn't
# atomic on the remove status). That's good enough for now.
INDEX=$(git branch -r | \
               (grep -E "$PROPOSED_PREFIX[0-9]+" \
                       || echo 0) | \
               sed -r "s,.*$PROPOSED_PREFIX([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
NEXT_INDEX=$(($INDEX + 1))
git push -u origin "$PROPOSED_BRANCH:${PROPOSED_PREFIX}$NEXT_INDEX/$PROPOSED_BRANCH"
