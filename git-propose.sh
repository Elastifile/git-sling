#!/bin/bash
set -eu
# Proposes a new branch for slinging onto staging.
#
# USAGE:
#
#   git checkout <my_branch>
#   git-propose.sh
#
# This will create a new remote branch sling/proposed/N/my_branch,
# where N is the order in the queue
#
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))

SLING_PREFIX="sling"
PROPOSED_PREFIX="proposed"

in_sling_dir() {
    (
        cd $SCRIPT_DIR
        # when running as via git alias, the env will contain
        # GIT_WORKDIR and such that make all git commands work on the
        # repo where the user ran the alias, so doing 'cd' here would
        # not help; that's why we manually remove the GIT-related env
        # vars
        clear_env="unset GIT_DIR GIT_WORK_TREE GIT_PREFIX"
        (bash -c "$clear_env ; $@")
        cd - > /dev/null
    )
}

fetch_sling() {
    in_sling_dir "git fetch -p"
}

check_for_upgrade() {
    in_sling_dir "$SCRIPT_DIR/check-for-upgrade.sh"
}

show_usage() {
    echo | cat <<EOF
Usage: git propose <target branch> [[--ticket=TICKET] | --dev-task] [-y] [--rebase] [--dry-run] [--no-upgrade-check] [--vip] [--source=SOURCE_PREFIX]

 --rebase             Propose to just rebase the current branch instead of merging it into the target branch
 --dry-run            Don't actually merge the changes; just check that rebase + prepush passes.
 --no-flatten         Don't flatten merge commits
 --(no-)upgrade-check Enable/disable automatic checking for a new version of git-sling
 --vip                Give this proposal a higher priority than normal (use with discretion).
 -y                   Assume 'yes' on all questions (non-interactive)
 --email=EMAIL        Use given email address instead of current git user.email

 --ticket=TICKET      Include the string TICKET in the branch names.
                      May be given multiple times, all given strings will be included in the branch name.
                      (For use with post-merge tools, e.g. bug trackers)

 --dev-task           Propose without naming a ticket

Pipeline options:
 --source=SOURCE_PREFIX

For example, to merge to master use:

> git propose master

Or to just check if your branch can rebase & build over 'my_integration', use:

> git propose my_integration --dry-run

(Note, you shouldn't add the 'origin/' prefix.)

> git propose master --ticket=EL-1234 --ticket=EL-987

EOF
}

abort_bad_name() {
    show_usage
    echo
    echo "ERROR: Not a valid branch name: $ONTO_BRANCH"
    exit 1
}

abort_unclean() {
    echo
    echo "ERROR: Working dir is dirty! Use stash or clean."
    exit 1
}

abort_not_rebased() {
    echo
    echo "ERROR: HEAD is not rebased over origin/$ONTO_BRANCH! Please rebase it before proposing."
    exit 1
}

prompt() {
    if $ASSUME_YES;
    then
        echo "(Assuming yes)"
    else
        echo -n "$1 (y/n): "
        read answer
        if echo "$answer" | grep -iq "^y" ;
        then
            echo ""
        else
            echo "Aborted"
            exit 1
        fi
    fi
}

validate_prefix() {
    prefix_regex='^[-a-zA-Z0-9_]+$'
    echo "$1" | grep -E $prefix_regex >/dev/null || (
        echo "Invalid prefix: '"$1"'"
        exit 1
    )
}

IS_DRY_RUN=false
UPGRADE_CHECK=false
ONTO_PREFIX="onto"
ONTO_BRANCH=""
IS_VIP=false
SOURCE_PREFIX=""
MOVE_BRANCH_MODE="base"
FLATTEN=true
ASSUME_YES=false
USE_GIT_EMAIL=true
DEV_TASK=false
TICKETS=""
for arg in "$@"; do
    case $arg in
        --rebase)
            MOVE_BRANCH_MODE="rebase"
            ;;
        --dry-run)
            ONTO_PREFIX="dry-run-onto"
            IS_DRY_RUN=true
            ;;
        --no-flatten)
            FLATTEN=false
            ;;
        --no-upgrade-check)
            UPGRADE_CHECK=false
            ;;
        --upgrade-check)
            UPGRADE_CHECK=true
            ;;
        --vip)
            IS_VIP=true
            ;;
        --source=*)
            RAW_SOURCE_PREFIX="${arg#*=}"
            validate_prefix "$RAW_SOURCE_PREFIX"
            SOURCE_PREFIX="prefix-$RAW_SOURCE_PREFIX/"
            shift
            ;;
        --ticket=*)
            TICKETS="${TICKETS} ${arg#*=}"
            shift
            ;;
        --dev-task)
            DEV_TASK=true
            ;;
        -y)
            ASSUME_YES=true
            ;;
        --email=*)
            EMAIL="${arg#*=}"
            shift
            USE_GIT_EMAIL=false
            ;;
        -*)
            show_usage
            echo "ERROR: Unknown or invalid option: $arg"
            exit 1
            ;;
        *)
            ONTO_BRANCH="$arg"
            ;;
    esac
done

if $DEV_TASK ; then
    if [ -z "$TICKETS" ] ; then
        echo "No ticket specified, CI may decide to create a ticket for you."
    else
        show_usage
        echo "ERROR: Can't specify both --dev-task and --ticket"
        exit 1
    fi
else
    if [ -z "$TICKETS" ] ; then
        show_usage
        echo "ERROR: Must use either --ticket or --dev-task"
        exit 1
    else
        echo "Tickets: $TICKETS"
    fi
fi

if [ $(uname) == "Darwin" ]; then
    SED=gsed
else
    SED=sed
fi

if [ -z "$ONTO_BRANCH" ]; then
    show_usage
    echo
    echo "ERROR: merge branch not specified."
    exit 1
fi

echo "In git repository: $(git rev-parse --show-toplevel). Fetching..."
git fetch -p &
$UPGRADE_CHECK && fetch_sling &
wait

$UPGRADE_CHECK && check_for_upgrade

set -o pipefail

git check-ref-format refs/heads/$ONTO_BRANCH > /dev/null || abort_bad_name
git branch -r | grep $ONTO_BRANCH > /dev/null || abort_bad_name

git describe --dirty --all | grep -E ".*-dirty$"  > /dev/null && abort_unclean
PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)
if [ "${TICKETS}" != "" ]; then
    PROPOSED_BRANCH="${PROPOSED_BRANCH}_$(echo ${TICKETS} | tr ' ' '_')"
fi
BASE_COMMIT="$(git log -1 origin/$ONTO_BRANCH --format=%h)"

# The index here gives an approximate ordering (because it isn't
# atomic on the remove status). That's good enough for now.
if $IS_VIP;
then
    NEXT_INDEX=1
else
    INDEX=$(git branch -r | \
                   (grep -E "$PROPOSED_PREFIX/[0-9]+" \
                           || echo 0) | \
                   ${SED} -r "s,.*$PROPOSED_PREFIX/([0-9]+).*,\1,g" | \
                   sort -g | \
                   tail -1)
    NEXT_INDEX=$((INDEX + 1))
fi

verify_email() {
    if ( echo "$1" | grep "\-at\-" );
    then
        echo "your email ($1) contains '-at-' /"
        echo " we don't support that!"
        exit 1
    fi
}

if $USE_GIT_EMAIL; then
    EMAIL=$(git config user.email)
fi
if [ $EMAIL == "" ]; then
    echo "Email cannot be empty"
    exit 1
fi
verify_email "$EMAIL"
ESCAPED_EMAIL=$(echo "$EMAIL" | ${SED} -s 's/@/-at-/g')

escape_branch() {
    local escape_char=","
    echo "$1" | grep "$escape_char" > /dev/null && (
        echo "Character: $escape_char is not allowed in branch names"
        exit 1
    )
    echo "$1" | sed "s/\//$escape_char/g"
}
if [ "$MOVE_BRANCH_MODE" == "base" ]; then
    if ! git branch --merged HEAD -r | grep " *origin/$ONTO_BRANCH\$"  > /dev/null ;
    then
        abort_not_rebased
    fi
    MOVE_BRANCH_PARAM="$BASE_COMMIT"
else
    MOVE_BRANCH_PARAM="$(escape_branch $PROPOSED_BRANCH)"
fi

if $FLATTEN ;
then
    MERGE_TYPE=""
else
    MERGE_TYPE="-keep"
fi

REMOTE_BRANCH="${SLING_PREFIX}/${SOURCE_PREFIX}${PROPOSED_PREFIX}/$NEXT_INDEX/$(escape_branch $PROPOSED_BRANCH)/$MOVE_BRANCH_MODE$MERGE_TYPE/$MOVE_BRANCH_PARAM/$ONTO_PREFIX/$(escape_branch $ONTO_BRANCH)/user/$ESCAPED_EMAIL"

COMMIT_COUNT=$(git log --oneline $BASE_COMMIT..HEAD | wc -l)

if [ $COMMIT_COUNT = "0" ] ; then
    echo
    echo "WARNING: No commits to send! Aborting."
    exit 1
elif [ $COMMIT_COUNT = "1" ] ; then
    if [ "${TICKETS}" != "" ]; then
        git commit --amend -m "$(git log -1 --format='%B') - ${TICKETS}"
    fi
fi

echo "Proposing: $PROPOSED_BRANCH"
echo "Commits:"
echo

git log --oneline $BASE_COMMIT..HEAD | cat

echo

if $IS_DRY_RUN ; then
    echo "Sending commits for dry run (will not change $ONTO_BRANCH)."
else
    prompt "Are these the commits you want to propose for $ONTO_BRANCH?"
fi

git push origin "HEAD:$REMOTE_BRANCH"

echo
echo "Pushed to: $REMOTE_BRANCH"
echo
echo "Proposal added to work queue. An email will be sent to $EMAIL when the server starts working on it. If the server is busy, this may take some time."
echo
echo "To unpropose, use: git push --delete origin $REMOTE_BRANCH"
