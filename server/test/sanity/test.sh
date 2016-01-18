#!/bin/bash
set -eu

script_dir=$(dirname $(realpath $0))
cd $script_dir

prepush="./tools/prepush.sh"

sling_dir=$script_dir/../../..

sling_server=$(stack path --project-root)/$(stack path --dist-dir)/build/sling-server-exe/sling-server-exe

remote=$(mktemp -d)
cd $remote
git init . --bare

serverdir=$(mktemp -d)
workdir=$(mktemp -d)
cd $workdir

git clone $remote work
cd work

fail() {
    echo "ERROR: $@"
    exit 1
}

cd_server() {
    echo "cd to server @ $serverdir/work"
    cd $serverdir/work
}

cd_client() {
    echo "cd to client @ $workdir/work"
    cd $workdir/work
}

run_cmd() {
    cmd="$@"
    logfile=$(mktemp)
    echo "> $cmd"
    $cmd &> $logfile && rm $logfile || (echo "^ Command failed $?, log=$logfile"; exit 1)
}

logit() {
    run_cmd git $@
}

delete_rejected_branches() {
    logit fetch -p
    git branch -r | grep -E "sling/rejected/[0-9]+/$testbranch" || fail "Expecting rejected branch!"
    rejected_branch=$(git branch -r | grep -E "sling/rejected/[0-9]+/$testbranch")
    logit push --delete origin $(echo "$rejected_branch" | cut -d'/' -f2-)
}

add_prepush() {
    echo "adding prepush script."
    mkdir -p $(dirname $prepush)
    echo 'echo "$@"' > $prepush
    chmod +x $prepush
    logit add $prepush
    # NOTE: couldn't get it to commit with a message that has whitespace :(
    # Shell escaping is fighting against us.
    logit commit -m add_prepush
}


echo "Working in: $workdir/work"

testbranch="test1"

logit checkout -b master
touch "initial"
logit add initial
logit commit -m"initial"
logit push -u origin master

add_commit_file() {
    filename=$(basename "$1")
    content="${2:-bla}"
    echo "$content" > "$filename"
    logit add "$filename"
    logit commit -m"$filename"
}

logit checkout -b $testbranch

add_commit_file 1
add_commit_file 2
add_commit_file 3

yes | run_cmd $sling_dir/git-propose.sh master

cd $serverdir
logit clone $remote work
cd work

echo "----------------------------------------------------------------------"

echo "Testing server in: $serverdir/work"
# Should fail, no tools/prepush script in repo
echo "Expecting failure..."
run_cmd $sling_server $prepush && fail "ERROR: Server should fail!"

echo "----------------------------------------------------------------------"

cd_client
delete_rejected_branches

add_prepush
yes | run_cmd $sling_dir/git-propose.sh master

echo "----------------------------------------------------------------------"

echo "Running server, expecting success..."
cd_server
# Should succeed
run_cmd $sling_server $prepush || fail "Server should succeed!"

echo "----------------------------------------------------------------------"

cd_client

logit fetch -p
git branch -r | grep -E "sling/propose/[0-9]+/$testbranch" && fail "Expecting proposal branch to be deleted!"

echo "Testing non-rebase proposal"

logit reset --hard HEAD^1

add_commit_file not_rebased

yes | run_cmd $sling_dir/git-propose.sh master && fail "Expecting propose to fail because not rebased!"

echo "Testing un-rebasable proposal"

logit reset --hard origin/master
add_commit_file unrebasable "client side"

yes | run_cmd $sling_dir/git-propose.sh master

echo "----------------------------------------------------------------------"

cd_server
logit checkout master
logit reset --hard origin/master

add_commit_file unrebasable "server side"
logit push

echo "Expecting failure..."
run_cmd $sling_server $prepush && fail "ERROR: Server should fail (bad rebase)!"

echo "----------------------------------------------------------------------"

cd_client
delete_rejected_branches

echo "Testing non-master onto"

logit checkout -b integration
logit reset --hard origin/master
logit reset --hard HEAD^1
logit push -u origin integration

logit checkout $testbranch
logit reset --hard origin/integration

add_commit_file integration_test

yes | run_cmd $sling_dir/git-propose.sh integration

echo "----------------------------------------------------------------------"

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --oneline origin/integration | grep "integration_test" || fail "Expected integration branch to include the new commit"
git log --oneline origin/master | grep "integration_test" && fail "Expected master branch to NOT include the new commit"


echo "----------------------------------------------------------------------"
echo '
 #####  #     #  #####   #####  #######  #####   #####
#     # #     # #     # #     # #       #     # #     #
#       #     # #       #       #       #       #
 #####  #     # #       #       #####    #####   #####
      # #     # #       #       #             #       #
#     # #     # #     # #     # #       #     # #     #
 #####   #####   #####   #####  #######  #####   #####
'
