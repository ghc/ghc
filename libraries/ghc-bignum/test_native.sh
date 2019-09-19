#!/bin/sh

# Exit the script on first error
set -e

echo "====================================="
echo "       Testing native backend"
echo "====================================="
stack test --flag ghc-bignum:test --flag ghc-bignum:native
