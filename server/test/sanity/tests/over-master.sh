
logit checkout master
logit reset --hard origin/master

# do stuff directly over master:

add_commit_file directly_on_master

yes | run_cmd $sling_propose master

cd_server

# Mis-configure git fetch on purpose (sling is supposed to repair it)
git config --unset remote.origin.fetch

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p

git log --format="%H %s" origin/master | grep "directly_on_master" || fail "Expected master branch to include the new commit"

