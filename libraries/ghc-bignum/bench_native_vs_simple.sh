#!/bin/sh
stack --stack-yaml stack.simple.yaml bench  --flag ghc-bignum:test --flag ghc-bignum:native
