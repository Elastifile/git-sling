echo "Testing non-rebase proposal"

cd_client

logit checkout -b source_target_1
add_commit_file source_target_1

yes | run_cmd $sling_propose master

echo "----------------------------------------------------------------------"

cd_server
logit checkout master
logit reset --hard origin/master

run_cmd $sling_server --target-prefix step-2 poll -- $prepush || fail "ERROR: Server should succeed!"


cd_client

logit fetch -p

echo "Current branches:"
git branch -r
echo "----------------"

git branch -a | grep 'step-2/.*/source_target_1' || fail "Expected branch to move to step-2"

cd_server

run_cmd $sling_server --target-prefix step-3 poll --source-prefix step-2 -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

echo "Current branches:"
git branch -r
echo "----------------"

! ( git branch -a | grep 'step-2/.*/source_target_1' ) || fail "Expected branch to be removed from step-2"
git branch -a | grep 'step-3/.*/source_target_1' || fail "Expected branch to move to step-3"


cd_server

run_cmd $sling_server poll --source-prefix step-3 -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

echo "Current branches:"
git branch -r
echo "----------------"

! ( git branch -a | grep 'step-2/.*/source_target_1' ) || fail "Expected branch to be removed from step-2"
! ( git branch -a | grep 'step-3/.*/source_target_1' ) || fail "Expected branch to be removed from step-3"

git log --format="%s" origin/master | grep source_target_1 || fail "Expected branch to be merged to master"
