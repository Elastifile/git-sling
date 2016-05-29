#!/bin/bash
set -euxo pipefail

# The script creates build machine for git-sling
# Should be run by user build
script_dir=$(dirname $(realpath $0))


function configure_cron() {
  echo "Configuring cron..."
  crontab -r || echo "crontab -r failed, ignoring."
  crontab -l | { cat; echo "* * * * * ssh-agent $script_dir/sling-build-cron.sh"; } | crontab -
  crontab -l | { cat; echo "0 0 * * * rm /tmp/core_*"; } | crontab -
}

function configure_sudoers() {
  echo "Configuring sudoers..."
  if ! sudo true ;
  then
    echo "Enter root password (for setting up sudoers):"
    su - root -c bash -c "echo 'build   ALL=(ALL)       NOPASSWD: ALL' >> /etc/sudoers"
  fi
}

function configure_mstp() {
  echo "Configuring email (msmtp)..."
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

  sudo chown build:build /opt/msmtp.conf
  sudo chmod 600 /opt/msmtp.conf

}

create_workdir() {
  echo "Creating workdir..."
  sudo mkdir -p /build-workdir
  sudo chown build:build /build-workdir
  sudo chmod 755 /build-workdir
}

configure_git_user() {
  echo "Setting up git user info..."
  git config --global user.name "prepush"
  git config --global user.email "elasti-prepush@elastifile.com"
  git config --global push.default simple
}

configure_stack() {
    echo "Configuring stack..."
    curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo
    sudo yum install -y stack
}

configure_ulimit() {
    echo "Configuring ulimit..."
    sudo sed -ri 's, *build .*nofile .*,,g' /etc/security/limits.conf
    sudo bash -c "echo 'build - nofile 65535' >> /etc/security/limits.conf"
    sudo sed -ri 's,fs.file-max.*,,g' /etc/sysctl.conf
    sudo bash -c "echo 'fs.file-max = 100000' >> /etc/sysctl.conf"
    sudo sysctl -p
}

configure_docker() {
    sudo usermod -a -G docker build
}

configure_config() {
  echo "Copying config from example"
  cp -v $script_dir/sling-config.sh.example $script_dir/sling-config.sh
}

main() {
  whoami | grep -E '^build$' || ( echo "Expecting to run under user 'build'"; exit 1)
  deps="docker-engine git zlib-devel"
  rpm -qi $deps || (
      sudo yum install -y epel-release
      sudo yum install -y $deps
  )
  configure_sudoers
  configure_docker
  configure_stack
  configure_mstp
  configure_cron
  create_workdir
  configure_git_user
  configure_ulimit
  configure_config
  echo "Done."
}

main "$@"
