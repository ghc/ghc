#!/usr/bin/env bash

set -e

grep -E -q 'RELEASE=NO' configure.ac ||
  grep -E -q '\[[0-9]+\.[0-9]+\.[0-9]+\]' configure.ac ||
  ( echo "error: configure.ac: GHC version number must have three components when RELEASE=YES."; exit 1 )
