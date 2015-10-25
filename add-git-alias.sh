#!/bin/bash -eu
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
git config --global alias.propose \!\""$SCRIPT_DIR/git-propose.sh"\"
