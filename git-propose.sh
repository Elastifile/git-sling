#!/bin/bash -eu
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

source $SCRIPT_DIR/sling-config.sh

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

suggest_upgrade() {
    upgrade_cmd="cd $SCRIPT_DIR ; git checkout master; git rebase"
    echo "You're in luck! A new version of sling (git propose) is available."
    echo "To upgrade, run:"
    echo $upgrade_cmd
    echo -n "Should I do this for you? (y/n) "
    read answer
    if echo "$answer" | grep -iq "^y" ;
    then
        in_sling_dir "$upgrade_cmd"
        echo "Please re-run your command now."
        exit 1
    else
        echo "Ok, but it may not work."
    fi
}

fetch_sling() {
    in_sling_dir "git fetch -p"
}

check_for_upgrade() {
    in_sling_dir "git log master..origin/master --decorate --oneline | grep master && suggest_upgrade || true"
}

show_usage() {
    echo "Usage: git propose <integration branch on remote> [--dry-run]"
    echo ""
    echo "For example, to merge to master use:"
    echo -e "\n\t> git propose master\n"
    echo "Or to just check if your branch can rebase & build over 'my_integration', use:"
    echo -e "\n\t> git propose my_integration --dry-run\n"
    echo "(Note, you shouldn't add the 'origin/' prefix.)"
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
    echo -n "$1 (y/n) "
    read answer
    if echo "$answer" | grep -iq "^y" ;
    then
        echo ""
    else
        echo "Aborted"
        exit 1
    fi
}

IS_DRY_RUN=false
ONTO_PREFIX="onto"
ONTO_BRANCH=""
for arg in "$@"; do
    case $arg in
        --dry-run)
            ONTO_PREFIX="dry-run-onto"
            IS_DRY_RUN=true
            ;;
        *)
            ONTO_BRANCH="$arg"
            ;;
    esac
done

if [ -z "$ONTO_BRANCH" ]; then
    show_usage
    echo
    echo "ERROR: target branch not specified."
    exit 1
fi

echo "Fetching..."
git fetch -p &
fetch_sling &
wait

check_for_upgrade

set -o pipefail

git check-ref-format refs/heads/$ONTO_BRANCH > /dev/null || abort_bad_name
git branch -r | grep $ONTO_BRANCH > /dev/null || abort_bad_name

git describe --dirty --all | grep -E ".*-dirty$"  > /dev/null && abort_unclean
PROPOSED_BRANCH=$(git rev-parse --abbrev-ref HEAD)

git branch --merged HEAD -r | grep " *origin/$ONTO_BRANCH\$"  > /dev/null || abort_not_rebased
BASE_COMMIT="$(git log -1 origin/$ONTO_BRANCH --format=%h)"

# The index here gives an approximate ordering (because it isn't
# atomic on the remove status). That's good enough for now.
INDEX=$(git branch -r | \
               (grep -E "$PROPOSED_PREFIX[0-9]+" \
                       || echo 0) | \
               ${SCRIPT_DIR}/sed.sh -r "s,.*$PROPOSED_PREFIX([0-9]+).*,\1,g" | \
               sort -g | \
               tail -1)
NEXT_INDEX=$(($INDEX + 1))
git config user.email | grep "\-at\-" && \
    ( echo "your email contains '-at-' /";
      echo " we don't support    that!";
      exit 1)
EMAIL=$(git config user.email | ${SCRIPT_DIR}/sed.sh -s 's/@/-at-/g')
REMOTE_BRANCH="${PROPOSED_PREFIX}$NEXT_INDEX/$PROPOSED_BRANCH/base/$BASE_COMMIT/$ONTO_PREFIX/$ONTO_BRANCH/user/$EMAIL"

COMMIT_COUNT=$(git log --oneline $BASE_COMMIT..HEAD | wc -l)

if [ $COMMIT_COUNT = "0" ] ; then
    echo
    echo "WARNING: No commits to send! Aborting."
    exit 1
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
echo "To unpropose, use: git unpropose $PROPOSED_BRANCH"
