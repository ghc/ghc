#!/bin/sh
cp "$2" "$3"
echo "$1:2:8: a located warning from an external tool"
echo "  with a continuation line"
echo "an unlocated line from an external tool"
