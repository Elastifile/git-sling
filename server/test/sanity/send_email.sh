#!/bin/bash

echo "SENDING FAKE EMAIL"
echo "$@"

# Without this, sometimes getting 'hClose: resource vanished':
sleep 0.1
