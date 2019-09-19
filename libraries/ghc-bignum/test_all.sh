#!/bin/sh

# Exit the script on first error
set -e

echo "====================================="
echo "       Testing native backend"
echo "====================================="
stack test --flag ghc-bignum:test --flag ghc-bignum:native

echo "====================================="
echo "        Testing GMP backend"
echo "====================================="
stack test --flag ghc-bignum:test --flag ghc-bignum:gmp

echo "====================================="
echo "        Validate GMP backend"
echo "====================================="
stack test --flag ghc-bignum:test --flag ghc-bignum:check --flag ghc-bignum:gmp

echo "====================================="
echo "      Building with FFI backend"
echo "====================================="
stack build --flag ghc-bignum:test --flag ghc-bignum:ffi
