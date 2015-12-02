#!/bin/bash -eu

realpath . &> /dev/null || echo "Please install realpath (OSX:brew install coreutils)"
if [ `uname` == "Darwin" ]; then
    echo | gsed -r "" &> /dev/null || echo "Please install gsed from brew (brew install gnu-sed)"
    echo | gxargs -r &> /dev/null || echo "Please install gxargs from brew (brew install findutils)"
fi

SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))
git config --global alias.propose \!\""$SCRIPT_DIR/git-propose.sh"\"
git config --global alias.unpropose \
    '!'"$SCRIPT_DIR"'/git-delete-sling-branch.sh proposed'
git config --global alias.delete-rejected \
    '!'"$SCRIPT_DIR"'/git-delete-sling-branch.sh rejected'
