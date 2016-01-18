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
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))

source $SCRIPT_DIR/sling-config.sh

ONTO_BRANCH="$1"
set -o pipefail

abort_bad_name() {
    echo
    echo "ERROR: Not a valid branch name: $ONTO_BRANCH"
    exit 1
}

abort_unclean() {
    echo
    echo "ERROR: Working dir is dirty! Use stash or clean."
    exit 1
}

abort_not_rebased() {
    echo
    echo "ERROR: HEAD is not rebased over origin/$ONTO_BRANCH! Please rebase it before proposing."
    exit 1
}

prompt() {
    echo -n "$1 (y/n) "
    read answer
    if echo "$answer" | grep -iq "^y" ;
    then
        echo ""
    else
        echo "Aborted"
        exit 1
    fi
}
git check-ref-format refs/heads/$ONTO_BRANCH || abort_bad_name
git branch -r | grep $ONTO_BRANCH || abort_bad_name

git describe --dirty --all | grep -E ".*-dirty$" && abort_unclean
PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)

echo "Fetching..."
git fetch
git branch --merged HEAD -r | grep " *origin/$ONTO_BRANCH\$"  > /dev/null || abort_not_rebased
BASE_COMMIT="$(git log -1 origin/$ONTO_BRANCH --format=%h)"

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
REMOTE_BRANCH="${PROPOSED_PREFIX}$PROPOSED_BRANCH/$NEXT_INDEX/$BASE_COMMIT/onto/$ONTO_BRANCH/$EMAIL"

echo "Proposing: $PROPOSED_BRANCH"
echo "Commits:"
echo

git log --oneline $BASE_COMMIT..HEAD | cat

echo

prompt "Are these the commits you want to propose for master?"

git push origin "HEAD:$REMOTE_BRANCH"

echo
echo "Pushed to: $REMOTE_BRANCH"
echo
echo "To unpropose, use: git unpropose $PROPOSED_BRANCH"
