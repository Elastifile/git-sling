#!/bin/bash -eu

PATTERN=$1
echo "Sling queue"
git remote show origin | grep proposed |  awk -F '/' '{print $4, $5, $6, $7, $8, $9, $10, $11, $12}' | sort -g |  grep --color -E "|^.*${PATTERN}.*$|"

echo "Done."
