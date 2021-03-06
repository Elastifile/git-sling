#!/bin/bash
set -eu

script_dir=$(dirname $(realpath $0))
cd $script_dir

prepush="./tools/prepush.sh"

sling_dir=$script_dir/../../..
send_email=$script_dir/send_email.sh
sling_server="$(stack path --project-root)/$(stack path --dist-dir)/build/sling/sling --email-client $send_email"
sling_propose="bash -x $sling_dir/git-propose.sh"

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

run_cmd_fail() {
    local cmd="$@"
    local logfile=$(mktemp)
    echo "> $cmd"
    set +e
    $cmd &> $logfile
    local result="$?"
    set -e
    if [ $result -eq "0" ]; then
        echo "^ Command succeeded (but expecting failure) $?, log=$logfile"
        exit 1
    fi
    rm $logfile
}

run_cmd() {
    local cmd="$@"
    local logfile=$(mktemp)
    echo "> $cmd > $logfile"
    $cmd &> $logfile || (echo "^ Command failed $?, log=$logfile"; exit 1)
}

logit() {
    run_cmd git --no-pager $@
}

delete_rejected_branches() {
    logit fetch -p
    git --no-pager branch -r | grep -E "sling/rejected/[0-9]+/$testbranch" || fail "Expecting rejected branch!"
    local rejected_branch
    rejected_branch=$(git --no-pager branch -r | grep -E "sling/rejected/[0-9]+/$testbranch")
    logit push --delete origin $(echo "$rejected_branch" | cut -d'/' -f2-)
}

add_prepush() {
    echo "adding prepush script."
    mkdir -p $(dirname $prepush)
    echo 'echo "$@"'       > $prepush
    echo 'echo "$@" 1>&2' >> $prepush
    chmod +x $prepush
    logit add $prepush
    # NOTE: couldn't get it to commit with a message that has whitespace :(
    # Shell escaping is fighting against us.
    logit commit -m add_prepush
}

add_commit_file() {
    local filename
    filename=$(basename "$1")
    local content
    content="${2:-bla}"
    local message
    message="${3:-$filename}"
    echo "$content" > "$filename"
    logit add "$filename"
    local message_file
    message_file="$(mktemp)"
    echo "$message" > $message_file
    logit commit -F $message_file
}
