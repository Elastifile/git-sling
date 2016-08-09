echo "Testing non-rebase proposal"

logit reset --hard HEAD^1

add_commit_file not_rebased

yes | run_cmd $sling_propose master && fail "Expecting propose to fail because not rebased!"

echo "Testing un-rebasable proposal"

logit reset --hard origin/master
add_commit_file unrebasable "client side"

yes | run_cmd $sling_propose master

echo "----------------------------------------------------------------------"

cd_server
logit checkout master
logit reset --hard origin/master

add_commit_file unrebasable "server side"
logit push

echo "Expecting failure..."
run_cmd_fail $sling_server poll -- $prepush || fail "ERROR: Server should fail (bad rebase)!"
