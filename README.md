# git-sling

Sling is a mini-CI tool for working with git. It includes two parts:

1. `git propose`: A client command for proposing new branches to be merged onto master (or onto some other integration branch).
2. sling server: Runs somewhere (usually a build server). The server rebases & tests incoming branches, and if they pass, merges them to master.


# Setup: Client

Run:

    git clone <git sling repository>
    cd git-sling
    ln -s $(realpath .)/git-propose ~/.local/bin/

This will make the following command available:

`git propose` - proposes the current branch. The server will (eventually) rebase & test this branch. If it passes, it will be merged to master. If it fails, the branch will be rejected.

You can use `git propose -h` for more information.

## Example workflow

A typical day at the office:

    git checkout -b my_feature
    ... hack hack hack ...
    git propose master

When the branch is picked up by the server, you'll receive an email. When it passes (or fails) you'll get another email.

# Setup: Server

## Building

Sling server is built using [Haskell Stack](http://www.haskellstack.org).

For convenience, there is an installation script: `install-server.sh`. It has only been tested on *Centos*, but you can probably fix it to work on X.

If you have stack installed, you can do:

    cd server
    stack install

Then for usage, run:

    sling-server-exe --help

## Running

The install script sets up a cron job, but you may want to use  initd/systemd/upstart service: setup contributions welcome!

# Contributors

Author: Noam Lewis ([@sinelaw](https://github.com/sinelaw))

OS X support for client: Eli Weissbrem
