#!/bin/bash
logit checkout master
logit reset --hard origin/master

master_hash=$(git rev-parse origin/master)

logit checkout -b take_my_job
add_commit_file take_my_freakin_job

yes | run_cmd $sling_propose master

cd_server

check_in_prog() {
    git branch -r | grep -E '^ *origin/sling/in-progress/.*/take_my_job/'
}

is_empty() {
    [[ "$(cat $1 | wc -l)" -eq "0" ]]
}

(
    set -e
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'shmaster' > $tmp_stdout

    is_empty $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"

    rm $tmp_stdout
)

! check_in_prog || fail "Expecting none in-progress"

(
    set -e
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'master' > $tmp_stdout

    [[ $(cat $tmp_stdout | wc -l) -eq 3 ]] || fail "Expecting yes a job, got: $tmp_stdout"
    branch_name=$(sed -n 1p $tmp_stdout)
    base_commit=$(sed -n 2p $tmp_stdout)
    head_commit=$(sed -n 3p $tmp_stdout)

    echo $branch_name | grep '/in-progress/.*take_my_job' || fail "Expected to take our job, took something else: $tmp_stdout"
    [[ "$base_commit" == "$master_hash" ]] || fail "Base didn't match master"
    [[ "$head_commit" == "$(git rev-parse origin/$branch_name)" ]] || fail "Head hash is wrong"

    rm $tmp_stdout
)

check_in_prog || fail "Expecting in progress"

(
    set -e
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'master' > $tmp_stdout

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
