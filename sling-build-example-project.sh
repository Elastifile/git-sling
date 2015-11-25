#!/bin/bash -eux
git submodule update --init
buildsome -j8 buildonly --overwrite
buildsome -j1 --overwrite
cd packaging
./build
