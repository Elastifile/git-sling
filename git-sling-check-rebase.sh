#!/bin/bash
set -eu

SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
( source $SCRIPT_DIR/sling-config.sh &> /dev/null ) || source $SCRIPT_DIR/sling-config.sh.example

git describe --dirty --all | grep -E ".*-dirty$"  > /dev/null && ( echo "Working directory unclean, can't run."; exit 1)

current_rev=$(git rev-parse HEAD)
current_branch=$(git rev-parse --abbrev-ref HEAD)
cleanup() {
    git rebase --abort >/dev/null 2>&1 || true
    git reset --hard $current_rev >/dev/null 2>&1
    git checkout $current_branch >/dev/null 2>&1
    git branch -D _sling_check_rebase_ >/dev/null 2>&1 || true
}

git fetch -p > /dev/null 2>&1

trap "cleanup" EXIT

git branch -D   _sling_check_rebase_ >/dev/null 2>&1 || true
git checkout -b _sling_check_rebase_ >/dev/null 2>&1
for branch in $( git branch -r | grep -E "^ *origin/${SLING_PREFIX}/(prefix-[^/]+/)(in-progress|${PROPOSED_PREFIX})/" );
do
    git rebase $branch >/dev/null 2>&1 || echo "Rebase over branch would fail: $branch"
    git rebase --abort >/dev/null 2>&1 || true
    git reset --hard $current_rev >/dev/null 2>&1
done
