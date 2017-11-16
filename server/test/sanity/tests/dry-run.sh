
logit checkout master
logit reset --hard origin/master

logit checkout -b dry_run_test
git push -u origin dry_run_test

add_commit_file dry_run_test

# with --dry-run, no need for piping 'yes'
run_cmd $sling_propose --dev-task dry_run_test --dry-run
run_cmd $sling_propose --dev-task master --dry-run


cd_server

echo "Expecting success..."
run_cmd      "$sling_server poll --match-branches             shooki        -- exit 1" || fail "ERROR: Server should succeed!"
run_cmd      "$sling_server poll --match-dry-run-branches     shooki        -- exit 1" || fail "ERROR: Server should succeed!"
run_cmd      "$sling_server poll --match-dry-run-branches     '^$'          -- exit 1" || fail "ERROR: Server should succeed!"
run_cmd_fail "$sling_server poll --match-dry-run-branches     dry_run_test  -- exit 1" || fail "ERROR: Server should fail!"
# already happened, should succeed:
run_cmd      "$sling_server poll --match-dry-run-branches     dry_run_test  -- exit 1" || fail "ERROR: Server should succeed!"
run_cmd_fail "$sling_server poll --match-branches             master        -- exit 1" || fail "ERROR: Server should fail!"

cd_client

logit fetch -p

! ( git log --format="%H %s" origin/master | grep "dry_run_test" ) || fail "Expected master branch to NOT include the new commit"


