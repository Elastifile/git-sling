
logit checkout master
logit reset --hard origin/master

logit checkout -b branch_not_matching
logit push -u origin branch_not_matching

logit checkout master
logit checkout -b test_not_matching
add_commit_file test_not_matching

yes | run_cmd $sling_propose branch_not_matching

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush --match-branches 'master' || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/branch_not_matching | grep "test_not_matching" && fail "Expected branch_not_matching branch to NOT include the new commit"
git log --format="%H %s" origin/master              | grep "test_not_matching" && fail "Expected master branch to NOT include the new commit"

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush --match-non-dry-run-branches '.*not_matching.*' || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/branch_not_matching | grep "test_not_matching" || fail "Expected branch_not_matching branch to YES include the new commit"
! ( git log --format="%H %s" origin/master          | grep "test_not_matching" ) || fail "Expected master branch to NOT include the new commit"

