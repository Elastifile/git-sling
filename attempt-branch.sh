#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="$1"
TARGET_BRANCH_PREFIX="$2"
COMMAND="$3"
SOURCE_BRANCH_NAME="$4"
REJECT_BRANCH_PREFIX="rejected/"
BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -e "s,^$SOURCE_BRANCH_PREFIX,,g")

git reset --hard && \
    git checkout $SOURCE_BRANCH_NAME || \
        (echo "checkout failed!" ; exit 1) && \
            $COMMAND || \
                (echo "rejecting: $BRANCH_NAME" && \
                        git checkout -b "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}" && \
                        git push -u origin "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}" && \
                        git branch -d "${SOURCE_BRANCH_NAME}" && \
                        git push --delete origin "${SOURCE_BRANCH_NAME}"; \
                 exit 1) && \

git checkout -b "${TARGET_BRANCH_PREFIX}${BRANCH_NAME}" && \
    git push -u origin "${TARGET_BRANCH_PREFIX}${BRANCH_NAME}" && \
    git branch -d "${SOURCE_BRANCH_NAME}" && \
    git push --delete origin "${SOURCE_BRANCH_NAME}"



