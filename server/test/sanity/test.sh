#!/bin/bash
set -eu

source ./test-funcs.sh

remote=$(mktemp -d)
cd $remote
git init . --bare

serverdir=$(mktemp -d)
workdir=$(mktemp -d)
cd $workdir

git clone $remote work
cd work
git config user.email "you@example.com"
git config user.name "Your Name"

echo "Working in: $workdir/work"
echo "Using sling: $sling_server"

# Setup test environment, test that it behaves as expected

testbranch="test1"

logit checkout -b master
touch "initial"
logit add initial
logit commit -m"initial"
logit push -u origin master

logit checkout -b $testbranch

add_commit_file 1
add_commit_file 2
add_commit_file 3

yes | run_cmd $sling_propose --dev-task master

cd $serverdir
logit clone $remote work
cd work
git config user.email "you@example.com"
git config user.name "Your Name"

echo "----------------------------------------------------------------------"

echo "Testing server in: $serverdir/work"
git --no-pager branch -r | grep rejected && fail "No rejected branches expected yet" || true

# Should fail, no tools/prepush script in repo
echo "Expecting failure..."
run_cmd_fail $sling_server poll -- $prepush || fail "ERROR: Server should fail!"

git --no-pager branch -r | grep rejected || fail "Server should have created a rejected branch and pushed it"

echo "----------------------------------------------------------------------"

cd_client
delete_rejected_branches

add_prepush
yes | run_cmd $sling_propose --dev-task master

echo "----------------------------------------------------------------------"

echo "Running server, expecting success..."
cd_server
# Should succeed
run_cmd $sling_server poll -- $prepush || fail "Server should succeed!"

echo "----------------------------------------------------------------------"

cd_client

logit fetch -p
! ( git --no-pager branch -r | grep -E "sling/propose/[0-9]+/$testbranch" ) || fail "Expecting proposal branch to be deleted!"

exec_test() {
    cd_client
    logit fetch -p
    logit checkout master
    logit reset --hard origin/master

    git --no-pager branch -r | grep proposed | cut -d/ -f2- | xargs -r git push --delete origin
    git branch | grep proposed | xargs -r git branch -D

    echo "----------------------------------------------------------------------"
    echo "Running test: $1"
    (
        source "$script_dir/$1"
    )
    echo "----------------------------------------------------------------------"
}


exec_test "tests/basic.sh"
exec_test "tests/bad-email.sh"
exec_test "tests/merge-commit.sh"
exec_test "tests/ticket.sh"
exec_test "tests/duplicate-propose.sh"
exec_test "tests/reject.sh"
exec_test "tests/take-job.sh"
exec_test "tests/rebase.sh"
exec_test "tests/onto.sh"
exec_test "tests/dry-run.sh"
exec_test "tests/over-master.sh"
exec_test "tests/matching.sh"
exec_test "tests/order.sh"
exec_test "tests/multi-propose.sh"
exec_test "tests/specific-proposal.sh"
exec_test "tests/source-target.sh"
exec_test "tests/in-progress-resume.sh"
exec_test "tests/flatten.sh"
exec_test "tests/deleted-remote-branch.sh"

echo '
 #####  #     #  #####   #####  #######  #####   #####
#     # #     # #     # #     # #       #     # #     #
#       #     # #       #       #       #       #
 #####  #     # #       #       #####    #####   #####
      # #     # #       #       #             #       #
#     # #     # #     # #     # #       #     # #     #
 #####   #####   #####   #####  #######  #####   #####
'
