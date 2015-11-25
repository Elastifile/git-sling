#!/bin/bash -eu

BRANCH="$1"
echo "Searching for rejected branches to delete with prefix $1"
git fetch \
  && git remote prune origin \
  && git branch -a \
  | grep sling \
  | grep -o "sling/rejected/$BRANCH.*" \
  | xargs -r -n1 --interactive git push --delete origin
echo "Done."
