#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="$1"
TARGET_BRANCH_PREFIX="$2"
COMMAND="$3"
SOURCE_BRANCH_NAME="$4"
REJECT_BRANCH_PREFIX="sling/rejected/"
STAGING="staging"
BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -e "s,^$SOURCE_BRANCH_PREFIX,,g")

reject() {
    echo "rejecting: $BRANCH_NAME"
    git checkout -b "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}"
    git push -u origin "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}"
    git branch -d "${SOURCE_BRANCH_NAME}"
    git push --delete origin "${SOURCE_BRANCH_NAME}"
    exit 1
}

rebase_failed() {
    git rebase --abort
    # TODO send email...
    exit 1
}

trap "reject" EXIT

git fetch
git reset --hard
git checkout $SOURCE_BRANCH_NAME
git rebase origin/staging || rebase_failed
git checkout staging
git merge --no-ff $SOURCE_BRANCH_NAME

$COMMAND

git push

trap - EXIT

git branch -D "${SOURCE_BRANCH_NAME}"
git push --delete origin "${SOURCE_BRANCH_NAME}"

