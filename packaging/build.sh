#!/bin/bash
set -eu
mock_config=${1:-epel-7-x86_64}
script_dir=$(dirname $(realpath $0))
cd $script_dir/..
echo "Using mock config: $mock_config"
$script_dir/build-srpm -o . -i sling -m $mock_config
