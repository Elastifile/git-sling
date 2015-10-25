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
index=$(git branch -r | \
               (grep -E "$PROPOSED_PREFIX[0-9]+" \
                       || echo 0) | \
               sed -r "s,.*$PROPOSED_PREFIX([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
git push -u origin "$PROPOSED_BRANCH:${PROPOSED_PREFIX}$(($index + 1))/$PROPOSED_BRANCH"
