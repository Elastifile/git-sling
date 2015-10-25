#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="$1"
COMMAND="$2"
SOURCE_BRANCH_NAME="$3"
STAGING="staging"
SLING_PREFIX="sling"
REJECT_BRANCH_PREFIX="$SLING_PREFIX/rejected/"
BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -e "s,^$SOURCE_BRANCH_PREFIX,,g")

abort() {
    # Go back to staging otherwise branch -d might fail.
    git reset --hard
    git checkout staging
    git branch -D "${SOURCE_BRANCH_NAME}" || echo "delete local branch failed, ignoring"
    exit 1
}
reject() {
    echo "rejecting: $BRANCH_NAME"
    (git checkout -b "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}" && \
            git push -u origin "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}") \
        || echo "Failed to create 'reject' branch - already exists?"
    git push --delete origin "${SOURCE_BRANCH_NAME}"
    abort
}

rebase_failed() {
    git rebase --abort
    # TODO send email...
    exit 1
}

trap "abort" EXIT

git fetch
git reset --hard

git checkout staging
git reset --hard origin/staging
git merge origin/master --ff-only
git push

git checkout $SOURCE_BRANCH_NAME


trap "reject" EXIT

git rebase origin/staging || rebase_failed
git checkout staging
git merge --no-ff $SOURCE_BRANCH_NAME

$COMMAND

trap "abort" EXIT

git push

git push --delete origin "${SOURCE_BRANCH_NAME}"

