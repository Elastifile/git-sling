#!/bin/bash
logit checkout master
logit reset --hard origin/master

master_hash=$(git rev-parse origin/master)

logit checkout -b take_my_job
add_commit_file take_my_freakin_job

yes | run_cmd $sling_propose --dev-task master

client_user_email=$(git config user.email)

cd_server

check_in_prog() {
    git --no-pager branch -r | grep -E '^ *origin/sling/in-progress/.*/take_my_job/'
}

is_empty() {
    [[ "$(cat $1 | wc -l)" -eq "0" ]]
}

(
    set -e
    tmp_stdout=$(mktemp)
    echo "......................................................................"
    echo "Running take-job ${LINENO}:"
    $sling_server take-job --match-branches 'shmaster' > $tmp_stdout

    is_empty $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"

    rm $tmp_stdout
)

! check_in_prog || fail "Expecting none in-progress"

(
    set -e
    tmp_stdout=$(mktemp)
    echo "......................................................................"
    echo "Running take-job ${LINENO}:"
    $sling_server take-job --match-branches 'master' > $tmp_stdout

    [[ $(cat $tmp_stdout | wc -l) -eq 5 ]] || fail "Expecting yes a job, got: $tmp_stdout"
    branch_name=$(sed -n 1p $tmp_stdout)
    base_commit=$(sed -n 2p $tmp_stdout)
    head_commit=$(sed -n 3p $tmp_stdout)
    user_email=$(sed -n 4p $tmp_stdout)
    onto_branch_name=$(sed -n 5p $tmp_stdout)

    echo $branch_name | grep '/in-progress/.*take_my_job' || fail "Expected to take our job, took something else: $tmp_stdout"
    [[ "$base_commit" == "$master_hash" ]] || fail "Base didn't match master"
    [[ "$head_commit" == "$(git rev-parse origin/$branch_name)" ]] || fail "Head hash is wrong"
    [[ "$user_email" == "$client_user_email" ]] || fail "email is wrong"
    [[ "$onto_branch_name" == "master" ]] || fail "onto branch name is wrong"
    rm $tmp_stdout
)

check_in_prog || fail "Expecting in progress"

(
    set -e
    tmp_stdout=$(mktemp)
    echo "......................................................................"
    echo "Running take-job ${LINENO}:"
    $sling_server take-job --no-concurrent --match-branches 'master' > $tmp_stdout

    is_empty $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"
    rm $tmp_stdout
)

check_in_prog || fail "Expecting in progress"

# ----------------------------------------------------------------------
cd_client
logit fetch -p
logit checkout master

! ls -1 take_my_freakin_job || fail "Not expecting the file yet"

logit rebase

! ls -1 take_my_freakin_job || fail "Not expecting the file yet"

# ----------------------------------------------------------------------
cd_server

# Make sure 'transition' starts on a clean slate, doesn't depend on
# state of master branch in this repo
logit checkout master
logit reset --hard origin/master

in_progress_proposal=$(check_in_prog | sed -r 's, *origin/,,g')
run_cmd $sling_server transition $in_progress_proposal || fail "Expecting success"

! check_in_prog || fail "Expecting none in-progress"

# ----------------------------------------------------------------------
cd_client

logit fetch -p
logit checkout master

! ls -1 take_my_freakin_job || fail "Not expecting the file yet"

logit rebase

ls -1 take_my_freakin_job || fail "Expecting the file"
