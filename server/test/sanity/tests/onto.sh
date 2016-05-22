echo "Testing non-master onto"

logit checkout -b integration
logit reset --hard origin/master
logit reset --hard HEAD^1
logit push -u origin integration

logit checkout $testbranch
logit reset --hard origin/integration

add_commit_file integration_test

yes | run_cmd $sling_propose integration

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/integration | grep "integration_test" || fail "Expected integration branch to include the new commit"

! ( git log --format="%H %s" origin/master | grep "integration_test" ) || fail "Expected master branch to NOT include the new commit"

