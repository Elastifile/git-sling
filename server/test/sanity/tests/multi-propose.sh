
echo "Testing multiple propose"

cd_client
logit fetch -p
logit checkout -b "multi_propose_A"
logit reset --hard origin/master
add_commit_file "multi_propose_A_file1"
add_commit_file "multi_propose_A_file2"

yes | run_cmd $sling_propose master

logit checkout -b "multi_propose_B"
logit reset --hard origin/master
add_commit_file "multi_propose_B_file1"
add_commit_file "multi_propose_B_file2"

yes | run_cmd $sling_propose master

logit checkout "multi_propose_A"
yes | run_cmd $sling_propose master

cd_server

echo "Expecting success..."
run_cmd $sling_server poll -- $prepush || fail "ERROR: Server should succeed!"

cd_client

logit fetch -p
commits_found=$(git log --format="%H %s" origin/master | grep 'multi_propose.*file' | wc -l)
if [ $commits_found -ne "4" ] ; then
    fail 'Expected 4 commits with multi_propose.*file'
fi

logit checkout master

files_found_before_rebase=$(ls -1 multi_propose* | wc -l)
if [ $files_found_before_rebase -ne "0" ] ; then
    fail 'Expected no files yet!'
fi

logit rebase

files_found_after_rebase=$(ls -1 multi_propose* | wc -l)
if [ $files_found_after_rebase -ne "4" ] ; then
    fail 'Expected 4 files!'
fi
