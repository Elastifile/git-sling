#!/bin/bash -eux
BASE_COMMIT="$(git log --format='%H' -1 $1)"
HEAD_COMMIT="$(git log --format='%H' -1 $2)"
SLING_DIR="$3"
if [[ -x ./prepush.sh ]]
then
    ./prepush.sh "$@"
else
    $SLING_DIR/elfs-prepush.sh "$@"
fi
