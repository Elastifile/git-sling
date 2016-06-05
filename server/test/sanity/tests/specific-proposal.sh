

echo "Deleting old client-local branches"

cd_client
logit fetch -p
logit branch -l | grep proposed | xargs -r git branch -D

echo "Testing attempt specific proposal"

cd_client
logit fetch -p
logit checkout master
logit reset --hard origin/master

logit checkout -b "proposal_in_queue_first"
add_commit_file "proposal_in_queue_first"

yes | run_cmd $sling_propose master

logit checkout master

logit checkout -b "specific_proposal"
add_commit_file "specific_proposal"

source_prefix="step-1"

yes | run_cmd $sling_propose --source=$source_prefix master

logit checkout master
specific_proposal_branch_name=$(git branch -r | grep "$source_prefix/proposed/.*specific_proposal" | cut -d/ -f2-)

cd_server

echo "Expecting success..."
run_cmd $sling_server --force-dry-run --source-prefix $source_prefix --proposal-branch $specific_proposal_branch_name -- $prepush || fail "Error: Server should succeed!"

cd_client

logit fetch -p
logit checkout master
logit reset --hard origin/master

! ( git log --format="%s" | grep 'proposal_in_queue_first' ) || fail "Error: Shouldn't have handled this proposal"
! ( git log --format="%s" | grep 'specific_proposal' ) || fail "Error: Shouldn't have handled this proposal"

! ( git branch -r | grep $specific_proposal_branch_name ) || fail "Error: specific proposal branch should be deleted"

logit checkout "specific_proposal"

yes | run_cmd $sling_propose --source=$source_prefix master

# Go back to server, run in without force dry run
cd_server

echo "Expecting success..."
run_cmd $sling_server --source-prefix $source_prefix --proposal-branch $specific_proposal_branch_name -- $prepush || fail "Error: Server should succeed!"

cd_client

logit fetch -p
logit checkout master
logit reset --hard origin/master

! ( git log --format="%s" | grep 'proposal_in_queue_first' ) || fail "Error: Shouldn't have handled this proposal"
git log --format="%s" | grep 'specific_proposal' || fail "Error: Did not handle the specific proposal"

