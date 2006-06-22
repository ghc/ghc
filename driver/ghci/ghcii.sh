#!/bin/sh
# Mini-driver for GHCi
exec "$0"/../ghc --interactive ${1+"$@"}
