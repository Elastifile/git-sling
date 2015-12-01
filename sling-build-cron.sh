#!/bin/bash -eu
ssh-add -D
ssh-add ~/.ssh/prepush_id_rsa
SLING_DIR=~/git/git-sling
test -d /build-workdir/elfs || (cd /build-workdir && git clone git@github.com:Elastifile/elfs.git)
cd $SLING_DIR
./cron.sh /build-workdir/elfs "$SLING_DIR/sling-build-elfs.sh"  &>> /tmp/build_elfs.log
