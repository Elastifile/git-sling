#!/bin/bash
logit checkout master
logit reset --hard origin/master

master_hash=$(git rev-parse origin/master)

logit checkout -b reject_me
add_commit_file reject_me

yes | run_cmd $sling_propose --dev-task master

cd_server

tmp_stdout=$(mktemp)

$sling_server take-job --match-branches 'master' > $tmp_stdout
proposal_branch_name=$(sed -n 1p $tmp_stdout)

rm $tmp_stdout

echo "rejecting: $proposal_branch_name"

$sling_server reject $proposal_branch_name "it literally stinks"

# ----------------------------------------------------------------------
cd_client

logit fetch -p

git --no-pager branch -r | grep '/rejected/.*reject_me' || fail "Expected branch to be rejected"
