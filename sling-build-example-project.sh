#!/bin/bash -eux
BASE_COMMIT="$1"
HEAD_COMMIT="$2"

git submodule update --init

build() {
    buildsome -j16 buildonly --overwrite "$@"
    buildsome -j4 --overwrite "$@"
}

GIT_SEQUENCE_EDITOR=true git rebase -i $BASE_COMMIT \
                   --exec 'buildsome -j16 buildonly --overwrite && buildsome -j4 --overwrite'

build --with stack_analysis
./testing/sanity/run.sh default
build --without optimization
buildsome -j16 buildonly --overwrite --with clang

# TODO mock build rpm
# cd packaging
# ./build
