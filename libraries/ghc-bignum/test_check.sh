#!/bin/sh

# Exit the script on first error
set -e

echo "====================================="
echo "        Validate GMP backend"
echo "====================================="
stack test --flag ghc-bignum:test --flag ghc-bignum:check --flag ghc-bignum:gmp
