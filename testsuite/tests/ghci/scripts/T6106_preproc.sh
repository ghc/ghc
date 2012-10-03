#!/bin/sh
#
# file T6106_preproc.sh
#
cat $2 > $3
sleep 1
echo "FAIL" >$1
