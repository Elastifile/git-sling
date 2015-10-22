# git-sling

Sling your branches onto master!

This is a simple utility for integrating branches into a common branch.

Branches are proposed by running `git-propose.sh branch_name`

Elsewhere, the script `git-sling.sh` runs (in a cron job, maybe) which
selects proposed branches and tries to rebase them onto `staging`, and
then tries to run a build/test command. If all goes well, `staging` is
pushed forward and the next proposed branch is attempted.
