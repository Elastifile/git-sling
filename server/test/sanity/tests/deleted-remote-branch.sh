#!/bin/bash

logit checkout master
logit reset --hard origin/master

logit checkout -b deleted_remote_branch
logit push -u origin deleted_remote_branch

add_commit_file delete_test "delete test"
yes | run_cmd $sling_propose --dev-task deleted_remote_branch

logit push --delete origin deleted_remote_branch

cd_server

echo "Expecting failure..."
run_cmd_fail "$sling_server poll -- echo Done" || fail "ERROR: Server should fail (bad branch)!"

cd_client

logit fetch -p
git --no-pager branch -r | grep 'rejected' | grep '/deleted_remote_branch/' || fail "Server should have created a rejected branch and pushed it"
