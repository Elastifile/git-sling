echo "Testing resumption of in-progress proposal that crashed"

cd_client
logit fetch -p

logit checkout -b "meddling_branch"
logit reset --hard origin/master
add_commit_file meddling_commit
echo 'bla' >> ./meddling_commit
logit add ./meddling_commit
logit commit -m"meddling"
yes | run_cmd $sling_propose --dev-task master


logit checkout -b "resume_in_progress"
logit reset --hard origin/master

add_commit_file resume_in_progress_commit_1_of_2

echo 'sleep 1000' > ./sleep_prepush.sh
chmod +x ./sleep_prepush.sh
logit add ./sleep_prepush.sh
logit commit -m'Add ./sleep_prepush.sh'

yes | run_cmd $sling_propose --dev-task master

cd_server

# merge the meddling branch
echo "Expecting success..."
git fetch
MEDDLING_PROPOSAL_BRANCH=$(git --no-pager branch -r | grep -E ' *origin/sling/.*proposed.*meddling' | cut -d/ -f2- )
run_cmd $sling_server proposal "${MEDDLING_PROPOSAL_BRANCH}" -- $prepush || fail "ERROR: Server should succeed!"

git --no-pager branch -r | grep '/in-progress/' && fail "In-progress branch should not exist!"

echo "Expecting timeout..."
run_cmd timeout 5 $sling_server poll -- ./sleep_prepush.sh && fail "ERROR: Server should have aborted on timeout!"

git --no-pager branch -r | grep '/in-progress/' || fail "In-progress branch not created!"

cd_server

# try again the other one
echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

git --no-pager branch -r | grep '/in-progress/' && fail "In-progress branch should not exist!"

cd_client

logit fetch -p
logit checkout master

test -e ./sleep_prepush.sh && fail "Should not exist yet"

logit rebase

test -e ./sleep_prepush.sh || fail "Should exist!"
