echo "Testing order"

cd_client
logit checkout -b "step0"
logit reset --hard origin/master

expected_steps_order=$(mktemp)
num_proposals=15
for i in $(seq 1 $num_proposals)
do
    logit checkout -b "step$i"
    logit reset --hard origin/master
    add_commit_file "step$i"
    echo "step$i" >> $expected_steps_order
    yes | run_cmd $sling_propose master
done

cd_server

echo "Expecting success..."
run_cmd $sling_server $prepush || fail "ERROR: Server should succeed!"

cd_client
logit fetch -p

logit checkout master
logit reset --hard origin/master
actual_steps_order=$(mktemp)
git log --format="%s" step0..origin/master  --reverse -- | grep -o 'step.*' > $actual_steps_order
run_cmd diff $expected_steps_order $actual_steps_order
rm $expected_steps_order
rm $actual_steps_order

