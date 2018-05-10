#!/usr/bin/python

import argparse
import re
import sys
import os
import os.path
import subprocess

SCRIPT_DIR = os.path.dirname(__file__)

EXAMPLE_HELP = '''
For example, to merge to master use:

> git propose master

Or to just check if your branch can rebase & build over 'my_integration', use:

> git propose my_integration --dry-run

(Note, you shouldn't add the 'origin/' prefix.)

> git propose master --ticket=EL-1234 --ticket=EL-987
'''

def build_parser():
    parser = argparse.ArgumentParser(
        description="Proposes a new branch for git-sling's server to process.",
        epilog=EXAMPLE_HELP)

    parser.add_argument(
        "onto_branch",
        metavar="TARGET_BRANCH",
        type=str,
        help="A (remote) branch name onto which the current branch should be rebased / merged (see --rebase / --dry-run / --no-flatten )")

    actions_group = parser.add_argument_group("Merge/rebase actions", "Instructions for git-sling's server on how to process this proprosal")
    actions_group.add_argument(
        "--rebase",
        default=False,
        action="store_true",
        help="Propose to just rebase the current branch instead of merging it into the target branch")
    actions_group.add_argument(
        "--dry-run",
        default=False,
        action="store_true",
        help="Don't actually merge the changes; just check that rebase + prepush passes")
    actions_group.add_argument(
        "--no-flatten",
        default=False,
        action="store_true",
        help="Don't flatten merge commits")
    actions_group.add_argument(
        "--vip",
        default=False,
        action="store_true",
        help="Give this proposal a higher priority than normal (use with discretion, you're cutting the queue!)")

    misc_group = parser.add_argument_group("Miscellaneous options")
    misc_group.add_argument(
        "--email",
        type=str,
        metavar="EMAIL",
        help="Use given email address instead of current git user.email (for email notifications)")
    misc_group.add_argument(
        "-y", "--yes",
        default=False,
        action="store_true",
        help="Assume 'yes' on all questions (non-interactive)")

    ticket_group = parser.add_mutually_exclusive_group()
    ticket_group.add_argument(
        "--ticket",
        type=str,
        action="append",
        help="Include the string TICKET in the branch names. May be given multiple times, all given strings will be included in the branch name. (For use with post-merge tools, e.g. bug trackers)")
    ticket_group.add_argument(
        "--dev-task",
        default=False,
        action="store_true",
        help="Propose without naming a ticket")

    pipeline_group = parser.add_argument_group("Pipeline options")
    pipeline_group.add_argument(
        "--source",
        type=str,
        metavar="SOURCE_PREFIX")

    return parser

def option_error(msg):
    sys.stderr.write("ERROR: " + msg + "\n")
    sys.exit(-1)

def validate_prefix(prefix):
    prefix_regex=r'^[-a-zA-Z0-9_]+$'
    if re.match(prefix_regex, prefix) is None:
        option_error("Invalid prefix: {}".format(prefix))

def cmd(args, error_msg=None):
    try:
        return subprocess.check_output(args, shell=False)
    except subprocess.CalledProcessError:
        if error_msg is not None:
            option_error(error_msg + ' (command failed: {})'.format(repr(' '.join(args))))
        raise

def cmd_lines(args, error_msg=None):
    return [x.strip()
            for x in cmd(args, error_msg=error_msg).split("\n")
            if x.strip() != ""]

def cmd_single_line(args, error_msg=None):
    res = cmd_lines(args, error_msg=error_msg)
    assert isinstance(res[0], str)
    assert len(res) == 1
    return res[0]

def escape_email(email):
    escaped_email = email.replace('@', '-at-')
    if escaped_email == email:
        option_error("your email ({}) contains '-at-' - we don't support that!".format(email))
    return escaped_email

def escape_branch(branch):
    escape_char = ','
    if escape_char in branch:
        option_error("Character: {} is not allowed in branch names".format(escape_char))
    return branch.replace("/", escape_char)

def none_to_empty_string(x):
    return '' if x is None else x

def echo(msg):
    sys.stdout.write(msg)
    sys.stdout.write("\n")

def prompt(msg, assume_yes):
    sys.stdout.write(msg + " (y/n): ")
    sys.stdout.flush()
    if assume_yes:
        sys.stdout.write("(assuming yes)\n")
        return
    if "y" != sys.stdin.readline().strip():
        echo("Aborted")
        sys.exit(1)

def main(parsed_args):
    if parsed_args.ticket is None:
        parsed_args.ticket = []

    cmd(["git", "check-ref-format", "refs/heads/{}".format(parsed_args.onto_branch)],
        "Not a valid branch name: {}".format(parsed_args.onto_branch))

    cmd(["git", "fetch"])
    remote_branches = cmd_lines(["git", "branch", "-r"])

    branch_regex = r"\b{}\b".format(re.escape('origin/' + parsed_args.onto_branch))
    for remote_branch in remote_branches:
        if re.match(branch_regex, remote_branch) is not None:
            break
    else:
        option_error("There is no remote branch with the name: {}".format(parsed_args.onto_branch))

    if re.match(".*-dirty$", cmd_single_line(["git", "describe", "--dirty", "--all"])) is not None:
        option_error("Working dir is dirty! Use stash or clean.")

    if (not parsed_args.dry_run) and (not parsed_args.rebase) and (not parsed_args.dev_task) and len(parsed_args.ticket) == 0:
        option_error("One of: --ticket= | --dev-task is required, unless using --dry-run | --rebase")

    proposed_branch = cmd_single_line(["git", "rev-parse", "--abbrev-ref", "HEAD"])
    for ticket in parsed_args.ticket:
        ticket_re = r"\b{}\b".format(re.escape(ticket))
        if re.match(ticket_re, proposed_branch) is None:
            proposed_branch += "_" + ticket.strip().replace(" ", "_")

    base_commit = cmd_single_line(["git", "log", "-1", "origin/{}".format(parsed_args.onto_branch), "--format=%h"]).strip()

    if parsed_args.vip:
        next_index = 1
    else:
        next_index = 1 + len([x for x in remote_branches if '/proposed/' in x])

    if parsed_args.email is None:
        email = cmd_single_line(["git", "config", "user.email"]).strip()
    else:
        email = parsed_args.email

    if parsed_args.rebase:
        move_branch_param = escape_branch(proposed_branch)
    else:
        merged_remote_branches = cmd_lines(["git", "branch", "--merged", "HEAD", "-r"])
        remote_onto_branch = "origin/{}".format(parsed_args.onto_branch)
        if remote_onto_branch not in merged_remote_branches:
            option_error("HEAD is not rebased over {}! Please rebase it before proposing.".format(remote_onto_branch))
        move_branch_param = base_commit

    if parsed_args.source is not None:
        source_prefix = 'prefix-{}/'.format(parsed_args.source)
    else:
        source_prefix = ''

    remote_branch="sling/{source_prefix}proposed/{next_index}/{escaped_propose_branch}/{move_branch_mode}{merge_type}/{move_branch_param}/{onto_prefix}/{escaped_onto_branch}/user/{escaped_email}".format(
        source_prefix = source_prefix,
        next_index = next_index,
        escaped_propose_branch = escape_branch(proposed_branch),
        move_branch_mode = "rebase" if parsed_args.rebase else "base",
        merge_type = "-keep" if parsed_args.no_flatten else "",
        move_branch_param = move_branch_param,
        onto_prefix = "dry-run-onto" if parsed_args.dry_run else "onto",
        escaped_onto_branch = escape_branch(parsed_args.onto_branch),
        escaped_email = escape_email(email)
        )

    commit_count = len(cmd_lines(["git", "log", "--oneline", "{}..HEAD".format(base_commit)]))
    if commit_count == 0:
        option_error("No commits to send! Aborting.")
    if commit_count == 1 and (len(parsed_args.ticket) > 0):
        orig_msg_lines = cmd_lines(["git", "log", "-1", "--format=%B"])
        missing_tickets = [
            ticket
            for ticket in parsed_args.ticket
            if re.match(r"\b{}\b".format(re.escape(ticket)), orig_msg_lines[0]) is None]
        if len(missing_tickets) > 0:
            tickets = " ".join(missing_tickets)
            new_subject = "{orig_msg} - {tickets}".format(orig_msg=orig_msg_lines[0].strip(), tickets=tickets)
            cmd(["git", "commit", "--amend", "-m", new_subject + "\n".join(orig_msg_lines[1:])])

    echo("Proposing: {}".format(proposed_branch))
    echo("Commits:")
    echo("")
    echo(cmd(["git", "log", "--oneline", "{}..HEAD".format(base_commit)]))
    echo("")
    if parsed_args.dry_run:
        echo("Sending commits for dry run (will not change {}).".format(parsed_args.onto_branch))
    else:
        prompt("Are these the commits you want to propose for {}?".format(parsed_args.onto_branch), parsed_args.yes)

    echo(cmd(["git", "push", "origin", "HEAD:{}".format(remote_branch)]))

    echo("""
Pushed to: {remote_branch}

Proposal added to work queue. An email will be sent to {email} when the server starts working on it. If the server is busy, this may take some time.

To unpropose, use:

   git push --delete origin {remote_branch}

""".format(email=email, remote_branch=remote_branch))

if __name__=='__main__':
    main(build_parser().parse_args())
