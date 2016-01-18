#!/bin/bash -eu
ssh-add -D
ssh-add ~/.ssh/prepush_id_rsa
SLING_DIR=~/git/git-sling
cd $SLING_DIR
./cron.sh &>> /tmp/build-sling.log
