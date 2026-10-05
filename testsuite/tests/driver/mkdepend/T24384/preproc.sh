#!/usr/bin/env bash

sed '/preproc/d' $2 > $3
echo 'import A' >> $3
