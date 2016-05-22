#!/bin/bash
set -eu

script_dir=$(dirname $(realpath $0))
cd $script_dir

prepush="./tools/prepush.sh"

sling_dir=$script_dir/../../..
send_email=$script_dir/send_email.sh
sling_server="$(stack path --project-root)/$(stack path --dist-dir)/build/sling-server-exe/sling-server-exe --email-client $send_email"
sling_propose="bash -x $sling_dir/git-propose.sh --no-upgrade-check"

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
    cmd="$@"
    logfile=$(mktemp)
    echo "> $cmd"
    set +e
    $cmd &> $logfile
    result="$?"
    set -e
    if [ $result -eq "0" ]; then
        echo "^ Command succeeded (but expecting failure) $?, log=$logfile"
        exit 1
    fi
    rm $logfile
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

add_commit_file() {
    filename=$(basename "$1")
    content="${2:-bla}"
    echo "$content" > "$filename"
    logit add "$filename"
    logit commit -m"$filename"
}
