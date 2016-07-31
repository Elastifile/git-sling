#!/bin/bash

echo "SENDING FAKE EMAIL" >> /tmp/email.txt
echo "$@"                 >> /tmp/email.txt
cat                       >> /tmp/email.txt
# Without this, sometimes getting 'hClose: resource vanished':
sleep 0.1
