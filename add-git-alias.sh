#!/bin/bash -eu
SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
git config --global alias.propose \!\""$SCRIPT_DIR/git-propose.sh"\"
git config --global alias.unpropose \
    '!'"$SCRIPT_DIR"'/git-delete-sling-branch.sh proposed'
git config --global alias.delete-rejected \
    '!'"$SCRIPT_DIR"'/git-delete-sling-branch.sh rejected'
