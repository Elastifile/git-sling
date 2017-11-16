
echo "Testing duplicate propose"

cd_client
logit fetch -p

logit checkout -b "duplicate_propose"
logit reset --hard origin/master
add_commit_file "double_propose_A_file1"
add_commit_file "double_propose_A_file2"

yes | run_cmd $sling_propose --dev-task master

cd_server

echo "Expecting success..."
run_cmd $sling_server take-job || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git branch -r |grep -E '/in-progress/.*duplicate_propose' || fail "Should have in-progress"

add_commit_file "double_propose_A_file3"

yes | run_cmd $sling_propose --dev-task master
yes | run_cmd $sling_propose --dev-task master

cd_server

echo "Expecting success..."
run_cmd $sling_server take-job || fail "ERROR: Server should succeed!"

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

logit checkout master

! ls double_propose* || fail "Files should not exist"

logit rebase

ls double_propose* || fail "Files should exist"
