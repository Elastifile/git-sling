#!/bin/bash

logit checkout master
logit reset --hard origin/master

ORIG_POS=$(git log --format="%H" -1)

logit checkout -b test_merge_commit origin/master~1
add_commit_file merge_commit_1

logit merge --no-ff origin/master

yes | run_cmd_fail $sling_propose --dev-task master || fail "git-propose should fail if there are merge commits!"

logit rebase origin/master

yes | run_cmd $sling_propose --dev-task master

cd_server

echo "Expecting success..."

run_cmd      "$sling_server poll -- echo Done" || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

for commit_name in merge_commit_1 ; do
    git log --format="%H %s" origin/master | grep $commit_name || fail "Expected master branch to include $commit_name"
done

MERGES_COUNT=$(git log --format="%H %s" $ORIG_POS..origin/master --merges | wc -l)

if [[ $MERGES_COUNT -ne "0" ]]; then
    echo "Expected MERGES_COUNT = 2 but got: $MERGES_COUNT"
    git log --oneline --decorate --graph
    exit 1
fi
