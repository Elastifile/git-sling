#!/bin/bash -eux
PROPOSED_PREFIX="sling/proposed/"
git fetch
index=$(git branch -r | \
               (grep -E "$PROPOSED_PREFIX[0-9]+" \
                       || echo 0) | \
               sed -r "s,.*$PROPOSED_PREFIX([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
git push -u origin "$1:${PROPOSED_PREFIX}$(($index + 1))/$1"
