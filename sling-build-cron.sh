#!/bin/bash -eu
ssh-add -D
ssh-add ~/.ssh/prepush_id_rsa
SLING_DIR=$(dirname $(realpath $0))
cd $SLING_DIR
./cron.sh &>> /tmp/build-sling.log
