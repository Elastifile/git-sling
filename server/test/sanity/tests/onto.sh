echo "Testing non-master onto"

logit checkout -b integration/base
logit reset --hard origin/master
logit reset --hard HEAD^1
logit push -u origin integration/base

logit checkout $testbranch
logit reset --hard origin/integration/base

add_commit_file integration_test

# This should do nothing:
! ( echo n | run_cmd $sling_propose integration/base ) || fail "Should exit with failure when given 'no'"

! ( git branch -r | grep 'sling/.*integration_test' ) || fail "Branch should not be proposed yet"

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

! ( git log --format="%H %s" origin/integration/base | grep "integration_test" ) || fail "Expected integration branch to not include the new commit"

# Check also the -y flag to propose:
run_cmd $sling_propose -y integration/base

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/integration/base | grep "integration_test" || fail "Expected integration branch to include the new commit"
! ( git log --format="%H %s" origin/master | grep "integration_test" ) || fail "Expected master branch to NOT include the new commit"

echo "----------------------------------------------------------------------"

logit checkout -b branch/with/slashes
logit reset --hard origin/integration/base
add_commit_file integration_best

yes | run_cmd $sling_propose integration/base

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/integration/base | grep "integration_best" || fail "Expected integration branch to include the new commit"
! ( git log --format="%H %s" origin/master | grep "integration_best" ) || fail "Expected master branch to NOT include the new commit"
