#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="$1"
COMMAND="$2"
SOURCE_BRANCH_NAME="$3"
STAGING="staging"
SLING_PREFIX="sling"
REJECT_BRANCH_PREFIX="$SLING_PREFIX/rejected/"
QUEUED_BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -e "s,^$SOURCE_BRANCH_PREFIX,,g")
BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -re "s,^$SOURCE_BRANCH_PREFIX[/0-9]*/(.*)/[^/]*,\1,g")
MSMTP_CONF_FILE="/opt/msmtp.conf"

PROPOSER_EMAIL=$(echo $SOURCE_BRANCH_NAME | \
                        sed -re 's,.*/([^/]+)$,\1,g' | \
                        sed -re 's,\-at\-,@,g' )
[ -z $PROPOSER_EMAIL ] && (echo "Failed parsing email, aborting. Branch: $SOURCE_BRANCH_NAME." ; exit 1)

MSMTP="msmtp -C $MSMTP_CONF_FILE $PROPOSER_EMAIL"

LOG_FILENAME=$(mktemp)

# Redirect stdout/stderr to logfile, taken from: http://stackoverflow.com/questions/637827/redirect-stderr-and-stdout-in-a-bash-script
# Close STDOUT file descriptor
exec 1<&-
# Close STDERR FD
exec 2<&-
# Open STDOUT as $LOG_FILE file for read and write.
exec 1<>$LOG_FILENAME
# Redirect STDERR to STDOUT
exec 2>&1

send_email() {
    MESSAGE="$@"
    RECEIPIENTS="$PROPOSER_EMAIL"
    BODY_FILE=$(mktemp)
    echo "Sending email to $RECEIPIENTS: $MESSAGE"
    echo "To: $RECEIPIENTS"          > $BODY_FILE
    echo "Subject: [sling] $QUEUED_BRANCH_NAME: $MESSAGE">> $BODY_FILE
    echo                            >> $BODY_FILE
    echo "$MESSAGE"                 >> $BODY_FILE
    echo                            >> $BODY_FILE
    echo "------------------------" >> $BODY_FILE
    echo "Tail of log: "            >> $BODY_FILE
    echo "------------------------" >> $BODY_FILE
    tail -100 $LOG_FILENAME         >> $BODY_FILE
    echo "------------------------" >> $BODY_FILE
    echo "Full Log: "               >> $BODY_FILE
    echo "------------------------" >> $BODY_FILE
    cat $LOG_FILENAME >> $BODY_FILE
    cat $BODY_FILE | $MSMTP $RECEIPIENTS || (echo "Failed sending email to $RECEIPIENTS")
    rm $BODY_FILE || true
}

abort() {
    send_email "Aborted"
    # Go back to staging otherwise branch -d might fail.
    git reset --hard
    git checkout staging
    git reset --hard origin/staging
    git branch -D "${SOURCE_BRANCH_NAME}" || echo "delete local branch failed, ignoring"
    exit 1
}



reject() {
    send_email "Rejecting"
    git fetch && git remote prune origin || echo "Fetch/prune failed: ignoring."
    (git checkout -b "${REJECT_BRANCH_PREFIX}${QUEUED_BRANCH_NAME}" && \
            git push -u origin "${REJECT_BRANCH_PREFIX}${QUEUED_BRANCH_NAME}") \
        || echo "Failed to create 'reject' branch - already exists?"
    git push --delete origin "${SOURCE_BRANCH_NAME}"
    abort
}

rebase_failed() {
    send_email "Rebase failed"
    git rebase --abort
    exit 1
}

trap "abort" EXIT

git fetch
git reset --hard

send_email "Attempting to merge"

git checkout staging
git reset --hard origin/staging
git merge origin/master --ff-only
git push

git branch -D $BRANCH_NAME || true
git checkout -b $BRANCH_NAME
git reset --hard origin/$SOURCE_BRANCH_NAME

trap "rebase_failed" EXIT
git rebase origin/staging
git checkout staging
git merge --no-ff $BRANCH_NAME
git commit --amend -s --author="$PROPOSER_EMAIL" -C HEAD

BASE_COMMIT=$(git log -1 --format=%H origin/staging)
HEAD_COMMIT=$(git log -1 --format=%H HEAD)

trap "reject" EXIT

$COMMAND $BASE_COMMIT $HEAD_COMMIT

trap "abort" EXIT

echo "Pushing updated staging."
git push

echo "Updating master."
git fetch
git checkout master
git reset --hard origin/master
git merge --ff-only origin/staging
git push

echo "Deleting branch ${SOURCE_BRANCH_NAME}"

git push --delete origin "${SOURCE_BRANCH_NAME}"

trap "" EXIT

send_email "Successfully merged"

