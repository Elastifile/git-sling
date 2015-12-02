#!/bin/bash -eu
if [ "$#" -eq 1 ]; then
    echo "Please specify branch pattern to search for (e.g. 'my_feature_branch')"
    exit 1
fi
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <proposed|rejected> <branch_pattern>"
    exit 1
fi
PREFIX="$1"
BRANCH="$2"
PATTERN="sling/$PREFIX/.*$BRANCH.*"
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
printf "Fetching..."
git fetch -q
printf "\rPruning..."
git remote prune origin
echo -e "\rSearching for branches to delete by pattern: \"$PATTERN\"..."
git branch -r | grep sling | grep -o "$PATTERN" \
    | ${SCRIPT_DIR}/xargs.sh -r -n1 --interactive git push --delete origin
echo "Done."
