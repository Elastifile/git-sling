#!/bin/bash -eux
#
# Proposes a new branch for slinging onto staging.
#
# USAGE:
#
#     git checkout my_branch
#     git-propose.sh
#
# This will do:
#
# 1. rebase your current branch onto $PROPOSED_STAGING and push it to
#    remote (to make sure other propositions are rebased on top of
#    you)
#
# 2. create a new remote branch sling/proposed/N/my_branch,
#    (where N is the order in the queue)
#
PROPOSED_PREFIX="sling/proposed/"
PROPOSED_STAGING="staging-proposed"

abort_unclean() {
    echo "Working dir is dirty! Use stash or clean."
    exit 1
}

abort_rebase_failed() {
    echo "Rebase failed. Please rebase on origin/$PROPOSED_STAGING manually."
    git rebase --abort || echo "(Oops, failed aborting rebase - leaving as-is.)"
    exit 1
}

git describe --dirty --all | (grep -E ".*-dirty$" && abort_unclean || echo "Working directory clean")

git fetch

INDEX=$(git branch -r | \
               (grep -E "$PROPOSED_PREFIX[0-9]+" \
                       || echo 0) | \
               sed -r "s,.*$PROPOSED_PREFIX([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
NEXT_INDEX=$(($INDEX + 1))

PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)

abort_return_to_branch() {
    echo "Aborting..."
    git checkout $PROPOSED_BRANCH
    exit 1
}

trap "abort_return_to_branch" EXIT

git rebase origin/$PROPOSED_STAGING || abort_rebase_failed
git checkout $PROPOSED_STAGING
git reset --hard origin/$PROPOSED_STAGING
git merge --no-ff $PROPOSED_BRANCH
# TODO check the merge only produced a nice linear merge
git push
git push origin "HEAD:${PROPOSED_PREFIX}$NEXT_INDEX/$PROPOSED_BRANCH"

git checkout $PROPOSED_BRANCH

