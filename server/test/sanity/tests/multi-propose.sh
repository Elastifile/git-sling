
echo "Testing multiple propose"

cd_client
logit fetch -p
logit checkout -b "double_propose_A"
logit reset --hard origin/master
add_commit_file "double_propose_A_file1"
add_commit_file "double_propose_A_file2"

yes | run_cmd $sling_propose master

logit checkout -b "double_propose_B"
logit reset --hard origin/master
add_commit_file "double_propose_B_file1"
add_commit_file "double_propose_B_file2"

yes | run_cmd $sling_propose master

logit checkout "double_propose_A"
yes | run_cmd $sling_propose master

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush || fail "ERROR: Server should succeed!"

