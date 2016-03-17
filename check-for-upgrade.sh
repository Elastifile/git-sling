#!/bin/bash
set -eu

SCRIPT_DIR=$(dirname $(realpath $BASH_SOURCE))

suggest_upgrade() {
    upgrade_cmd="cd $SCRIPT_DIR ; git checkout master; git rebase"
    echo "You're in luck! A new version of sling (git propose) is available."
    echo ""
    echo "WARNING: You should be sure the server has been upgraded before upgrading the client!"
    echo ""
    echo "To upgrade, run:"
    echo $upgrade_cmd
    echo -n "Should I do this for you? (y/n) "
    read answer
    if echo "$answer" | grep -iq "^y" ;
    then
        bash -c "$upgrade_cmd"
        echo "DONE!"
        echo "Please re-run your command now."
        exit 1
    else
        echo "Ok, but it may not work."
    fi
}

git log master..origin/master --decorate --oneline -- "*.sh" | grep master && suggest_upgrade || true
