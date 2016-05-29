#!/bin/bash -eux
git_root=$(git rev-parse --show-toplevel)
cd $git_root
target="$1"
target_host=$(echo $target | sed -r 's/([^@]+@)?(.*)/\2/g')
keyfile=$(mktemp)

sling_user="sling"
workdir="/build-workdir"
git_remote_url="https://github.com/Elastifile/git-sling.git"
# $(git config remote.origin.url)

trap "rm $keyfile" EXIT

keyfile_name=$(basename $keyfile)
ssh-keygen -t rsa -f $keyfile

on_remote() {
    ssh $target "$@"
}
on_remote_as_build() {
    ssh -i $keyfile $sling_user@$target_host "$@"
}

on_remote "getent passwd $sling_user || sudo useradd -b $workdir $sling_user"
on_remote "sudo su - $sling_user bash -c 'mkdir -p \$HOME/.ssh && chmod 700 \$HOME/.ssh'"
build_homedir=$(on_remote 'getent passwd $sling_user | cut -d: -f6')
scp $keyfile.pub $target:/tmp
on_remote "sudo su - $sling_user bash -c 'cat /tmp/$keyfile_name.pub >> \$HOME/.ssh/authorized_keys && chmod 600 \$HOME/.ssh/authorized_keys'"
on_remote "rm /tmp/$keyfile_name.pub"
on_remote "sudo chown $sling_user $workdir"

sling_user_keyfile="prepush_id_rsa"
on_remote_as_build "( test -e \$HOME/.ssh/$sling_user_keyfile || ssh-keygen -qt rsa -f \$HOME/.ssh/$sling_user_keyfile ) && cat \$HOME/.ssh/$sling_user_keyfile.pub"
on_remote_as_build "cd $workdir && ( cd git-sling && ( git stash || true ) && git fetch -p && git checkout master && git reset --hard origin/master ) || git clone $git_remote_url"
on_remote_as_build "cd $workdir/git-sling && ./install-server.sh"
