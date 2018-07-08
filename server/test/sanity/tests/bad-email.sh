logit checkout master
logit reset --hard origin/master

orig_email=$(git config user.email)

git config user.email "comma@domain,com"

add_commit_file email_with_comma

yes | run_cmd $sling_propose --dev-task master && fail "Expecting propose to fail because email is invalid"

git config user.email "$orig_email"

yes | run_cmd $sling_propose --dev-task master

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/master | grep "email_with_comma" || fail "Expected master branch to include the new commit"
