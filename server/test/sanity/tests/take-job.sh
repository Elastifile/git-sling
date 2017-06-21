logit checkout master
logit reset --hard origin/master

logit checkout -b take_my_job
add_commit_file take_my_freakin_job

yes | run_cmd $sling_propose master

cd_server

check_in_prog() {
    git branch -r | grep -E '^ *origin/sling/in-progress/.*/take_my_job/'
}

(
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'shmaster' > $tmp_stdout

    grep -E '^<<<.*no job taken' $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"
    ! grep -E '^>>>' $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"

    rm $tmp_stdout
)

! check_in_prog || fail "Expecting none in-progress"

(
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'master' > $tmp_stdout

    ! grep -E '^<<<.*no job taken' $tmp_stdout || fail "Expecting yes a job, got: $tmp_stdout"
    grep -E '^>>>' $tmp_stdout || fail "Expecting yes a job, got: $tmp_stdout"
    grep -E '>>> base:([^ ]+) head:([^ ]+) branch:(.*)' $tmp_stdout || fail "Failed to parse job, see $tmp_stdout"

    rm $tmp_stdout
)

check_in_prog || fail "Expecting in progress"

(
    tmp_stdout=$(mktemp)
    $sling_server take-job --match-branches 'master' > $tmp_stdout

    grep -E '^<<<.*no job taken' $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"
    ! grep -E '^>>>' $tmp_stdout || fail "Expecting no job, got: $tmp_stdout"
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
