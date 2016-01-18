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

cd $GIT_ROOT

GIT_SEQUENCE_EDITOR=true git rebase -p -i $BASE_COMMIT \
                   --exec 'buildsome -j8 buildonly --overwrite && buildsome -j4 --overwrite' \
    || (git rebase --abort ; exit 1)

with_workdir "stack_analysis"
build --with stack_analysis
./testing/sanity/run.sh default

with_workdir "debug"
build --without optimization

with_workdir "clang"
buildsome -j16 buildonly --overwrite --with clang

cd $GIT_ROOT

trap "" EXIT

# TODO mock build rpm
# cd packaging
# ./build