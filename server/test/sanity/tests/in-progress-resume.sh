
echo "Testing resumption of in-progress proposal that crashed"

cd_client
logit fetch -p
logit checkout -b "resume_in_progress"
logit reset --hard origin/master

add_commit_file resume_in_progress_commit_1_of_2

echo 'sleep 1000' > ./sleep_prepush.sh
chmod +x ./sleep_prepush.sh
logit add ./sleep_prepush.sh
logit commit -m"Add ./sleep_prepush.sh"

yes | run_cmd $sling_propose master

cd_server

git branch | grep in_progress && fail "In-progress branch should not exist!"

echo "Expecting timeout..."
run_cmd timeout 5 $sling_server poll -- ./sleep_prepush.sh && fail "ERROR: Server should have aborted on timeout!"

git branch | grep in_progress || fail "In-progress branch not created!"

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

git branch | grep in_progress && fail "In-progress branch should not exist!"

cd_client

logit fetch -p
logit checkout master

test -e ./sleep_prepush.sh && fail "Should not exist yet"

logit rebase

test -e ./sleep_prepush.sh || fail "Should exist!"

