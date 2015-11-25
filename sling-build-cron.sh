#!/bin/bash -eu
ssh-add -D
ssh-add ~/.ssh/prepush_id_rsa
SLING_DIR=~/git/git-sling
test -d /mnt/tmpfs/elfs || (cd /mnt/tmpfs && git clone git@github.com:Elastifile/elfs.git)
cd $SLING_DIR
./cron.sh /mnt/tmpfs/elfs "buildsome"  &>> /tmp/build_elfs.log
