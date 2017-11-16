
logit checkout master
logit reset --hard origin/master

# Create a staging branch with one commit in it saying "put_it_in_my_staging"

logit checkout -b rebase_staging
add_commit_file put_it_in_my_staging
logit push -u origin rebase_staging

# Move master

logit checkout master
add_commit_file master_moving_along
logit push

# Tell sling to rebase staging branch over master

logit checkout rebase_staging
yes | run_cmd $sling_propose --dev-task --rebase master

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

check_for_commit() {
    local branch="$1"
    local msg="$2"
    echo "Checking branch $branch for commit: $msg"
    git log --format="%H %s" origin/$branch | grep "$msg" &> /dev/null
}

check_for_commit "rebase_staging" "master_moving_along"  || fail "missing"
check_for_commit "rebase_staging" "put_it_in_my_staging" || fail "missing"
check_for_commit "master" "master_moving_along"          || fail "missing"
( ! check_for_commit "master" "put_it_in_my_staging" )   || fail "shouldn't be here"
