#!/bin/bash

# The script creates build machine for git-sling
# Should be run by user build

function clone_git_sling_repo() {

  test -d ~/git/git-sling || ( mkdir -p ~/git ; cd ~/git ; git clone git@github.com:Elastifile/git-sling.git )

}


function configure_cron() {

  crontab -r
  crontab -l | { cat; echo "* * * * * ssh-agent /home/build/git/git-sling/sling-build-cron.sh"; } | crontab -
  crontab -l | { cat; echo "0 0 * * * rm /tmp/core_*"; } | crontab -

}

function configure_mstp() {

  user="elasti-prepush@elastifile.com"
  echo -n "Please provide gmail password for user $user: "
  read -s password
  sudo yum -y install msmtp
  cat << EOF | sudo tee /opt/msmtp.conf >/dev/null
defaults
tls on
tls_starttls on
tls_trust_file /etc/pki/tls/certs/ca-bundle.crt

account default
host smtp.gmail.com
port 587
auth on
user $user
password $password
from $user
logfile /dev/null
EOF

  sudo chown build:rnd /opt/msmtp.conf
  sudo chmod 600 /opt/msmtp.conf

}

create_workdir() {
  sudo mkdir -p /build-workdir
  sudo chown build:rnd /build-workdir
  sudo chmod 755 /build-workdir
}

configure_git_user() {
  git config --global user.name "prepush"
  git config --global user.email "elasti-prepush@elastifile.com"
}

main() {

  configure_mstp
  clone_git_sling_repo
  configure_cron
  create_workdir
  configure_git_user
}

main "$@"
