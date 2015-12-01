#!/bin/bash -eu
ssh-add -D
ssh-add ~/.ssh/prepush_id_rsa
SLING_DIR=~/git/git-sling
test -d /build-workdir/example-project || (cd /build-workdir && git clone git@github.com:Elastifile/example-project.git)
cd $SLING_DIR
./cron.sh /build-workdir/example-project "$SLING_DIR/sling-build-example-project.sh"  &>> /tmp/build_example-project.log
