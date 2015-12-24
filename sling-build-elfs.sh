#!/bin/bash -eux
BASE_COMMIT="$(git log --format='%H' -1 $1)"
HEAD_COMMIT="$(git log --format='%H' -1 $2)"
SLING_DIR="$3"
./tools/prepush.sh "$BASE_COMMIT" "$HEAD_COMMIT"