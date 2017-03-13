#!/bin/bash

logit checkout master
logit reset --hard origin/master

ORIG_POS=$(git log --format="%H" -1)

logit checkout -b flatten_test_staging
add_commit_file flatten_test0

logit checkout -b flatten_test_other
git reset --hard master
logit push -u origin flatten_test_other

add_commit_file flatten_test1
add_commit_file flatten_test2

logit merge --no-ff flatten_test_staging

add_commit_file flatten_test3

yes | run_cmd $sling_propose --no-flatten master

cd_server

echo "Expecting success..."

run_cmd      "$sling_server poll -- echo Done" || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

for commit_name in flatten_test1 flatten_test2 flatten_test3 ; do
    git log --format="%H %s" origin/master | grep $commit_name || fail "Expected master branch to include $commit_name"
done

MERGES_COUNT=$(git log --format="%H %s" $ORIG_POS..origin/master --merges | wc -l)

if [[ $MERGES_COUNT -ne "2" ]]; then
    echo "Expected MERGES_COUNT = 2 but got: $MERGES_COUNT"
    exit 1
fi

