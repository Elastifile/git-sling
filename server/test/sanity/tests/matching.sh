
logit checkout master
logit reset --hard origin/master

logit checkout -b branch_not_matching
logit push -u origin branch_not_matching

logit checkout master
logit checkout -b test_not_matching
add_commit_file test_not_matching

yes | run_cmd $sling_propose --dev-task branch_not_matching

cd_server

echo "Expecting success..."
run_cmd $sling_server poll --match-branches 'master' -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/branch_not_matching | grep "test_not_matching" && fail "Expected branch_not_matching branch to NOT include the new commit"
git log --format="%H %s" origin/master              | grep "test_not_matching" && fail "Expected master branch to NOT include the new commit"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll --exclude-branches 'branch_not' -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/branch_not_matching | grep "test_not_matching" && fail "Expected branch_not_matching branch to NOT include the new commit"
git log --format="%H %s" origin/master              | grep "test_not_matching" && fail "Expected master branch to NOT include the new commit"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll --match-non-dry-run-branches '.*not_matching.*' -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/branch_not_matching | grep "test_not_matching" || fail "Expected branch_not_matching branch to YES include the new commit"
! ( git log --format="%H %s" origin/master          | grep "test_not_matching" ) || fail "Expected master branch to NOT include the new commit"

