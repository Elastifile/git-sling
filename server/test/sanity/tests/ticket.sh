echo "Testing ticket"

logit checkout -b ticket_test
logit reset --hard origin/master

add_commit_file ticket_test blabla "ticket_test

after newline
after newline"

yes | run_cmd $sling_propose                                            master && fail "Shouldn't require either --ticket or --dev-task"
yes | run_cmd $sling_propose --dev-task --ticket=AB-123 --ticket=AB-456 master && fail "Shouldn't allow both flags"
yes | run_cmd $sling_propose            --ticket=AB-123 --ticket=AB-456 master

logit checkout -b ticket_test_2
logit reset --hard origin/master

add_commit_file ticket_test_2A
add_commit_file ticket_test_2B
add_commit_file ticket_test_2C

yes | run_cmd $sling_propose --ticket=AB-987 master

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

echo "----------------------------------------------------------------------"

cd_client

logit fetch -p

logit checkout master

old_master=$(git rev-parse HEAD)
logit rebase

if git log "$old_master..HEAD" --format="%s" | grep 'after newline' ;
then
    fail "Didn't expect to find the part after the newline in %s"
fi
git log "$old_master..HEAD" --format="%s" | grep 'AB-123' || fail "Didn't find both ticket strings"
git log "$old_master..HEAD" --format="%s" | grep 'AB-456' || fail "Didn't find both ticket strings"
git log "$old_master..HEAD" --format="%s" | grep 'AB-987' || fail "Didn't find both ticket strings"

# check that dry-run doesn't need --ticket / --dev-task

add_commit_file ticket_dry_run_test

yes | run_cmd $sling_propose --dry-run master

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

echo "----------------------------------------------------------------------"

cd_client

echo "----------------------------------------------------------------------"
