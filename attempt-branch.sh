#!/bin/bash -eux

SOURCE_BRANCH_PREFIX="$1"
COMMAND="$2"
SOURCE_BRANCH_NAME="$3"
STAGING="staging"
SLING_PREFIX="sling"
REJECT_BRANCH_PREFIX="$SLING_PREFIX/rejected/"
BRANCH_NAME=$(echo "$SOURCE_BRANCH_NAME" | sed -e "s,^$SOURCE_BRANCH_PREFIX,,g")

MSMTP_CONF_FILE="/opt/msmtp.conf"

PROPOSER_EMAIL=$(echo $SOURCE_BRANCH_NAME | \
                        sed -re 's,.*/([^/]+)$,\1,g' | \
                        sed -re 's,\-at\-,@,g' )
[ -z $PROPOSER_EMAIL ] && (echo "Failed parsing email, aborting. Branch: $SOURCE_BRANCH_NAME." ; exit 1)

MSMTP="msmtp -C $MSMTP_CONF_FILE $PROPOSER_EMAIL"

LOG_FILENAME=$(mktemp)

send_email() {
    MESSAGE="$@"
    RECEIPIENTS="$PROPOSER_EMAIL"
    BODY_FILE=$(mktemp)
    echo "Sending email to $RECEIPIENTS: $MESSAGE"
    echo "To: $RECEIPIENTS"          > $BODY_FILE
    echo "Subject: [sling] $MESSAGE">> $BODY_FILE
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
    # Go back to staging otherwise branch -d might fail.
    git reset --hard
    git checkout staging
    git reset --hard origin/staging
    git branch -D "${SOURCE_BRANCH_NAME}" || echo "delete local branch failed, ignoring"
    exit 1
}
reject() {
    send_email "rejecting: $BRANCH_NAME"
    (git checkout -b "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}" && \
            git push -u origin "${REJECT_BRANCH_PREFIX}${BRANCH_NAME}") \
        || echo "Failed to create 'reject' branch - already exists?"
    git push --delete origin "${SOURCE_BRANCH_NAME}"
    abort
}

rebase_failed() {
    git rebase --abort
    send_email "SLING: Rebase failed: $BRANCH_NAME"
    # TODO send email...
    exit 1
}

trap "abort" EXIT

git fetch
git reset --hard

send_email "SLING: Attempting to merge: $BRANCH_NAME"

git checkout staging
git reset --hard origin/staging
git merge origin/master --ff-only
git push

git checkout $SOURCE_BRANCH_NAME

trap "reject" EXIT

git rebase origin/staging || rebase_failed
git checkout staging
git merge --no-ff $SOURCE_BRANCH_NAME


$COMMAND &> $LOG_FILENAME

trap "abort" EXIT

send_email "SLING: Successfully merged branch $BRANCH_NAME"

git push
git push --delete origin "${SOURCE_BRANCH_NAME}"

