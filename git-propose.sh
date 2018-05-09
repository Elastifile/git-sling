#!/bin/bash
set -eu
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
python ${SCRIPT_DIR}/git-propose.py $@
