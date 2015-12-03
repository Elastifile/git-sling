#!/bin/bash -eux
BASE_COMMIT="$(git log --format='%H' -1 $1)"
HEAD_COMMIT="$(git log --format='%H' -1 $2)"
WORKDIR_BASE="/tmp/workdir"
GIT_ROOT=$(git rev-parse --show-toplevel)

mkdir -p $WORKDIR_BASE

git submodule update --init

abort() {
    cd $GIT_ROOT
    exit 1
}

with_workdir() {
    NAME="$1"
    cd $WORKDIR_BASE
    mkdir -p $NAME
    cd $NAME
    test -d .git || git clone $GIT_ROOT .
    git fetch
    git reset --hard $HEAD_COMMIT
}

build() {
    buildsome -j16 buildonly --overwrite "$@"
    buildsome -j4 --overwrite "$@"
}

trap "abort" EXIT

GIT_SEQUENCE_EDITOR=true git rebase -i $BASE_COMMIT \
                   --exec 'buildsome -j8 buildonly --overwrite && buildsome -j4 --overwrite' \
    || (git rebase --abort ; exit 1)

# stack_analysis is partially incremental over regular build (only linkage)
build --with stack_analysis
./testing/sanity/run.sh default

with_workdir "debug"
build --without optimization

with_workdir "clang"
buildsome -j16 buildonly --overwrite --with clang

trap "" EXIT

# TODO mock build rpm
# cd packaging
# ./build
