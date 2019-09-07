# integer-openssl

Implementation of the `GHC.Integer` type using the OpenSSL BIGNUM arbitrary size
integer library contained in `libcrypto`.

## Work in progress

This is currently in development and not yet ready for production. Integration
with GHC is discussed
[here](https://github.com/ghc-proposals/ghc-proposals/pull/183). If things work
out, this would be a free BSD-licensed alternative to the LGPL-licensed
`integer-gmp` - used by default in GHC - with performance significantly faster
than `integer-simple` - a BSD-licensed Haskell-only integer library.

The test suite gives a rough estimate what currently works as each function is
checked against the builtin library.

Recent benchmarks of small, 128bit and 4096bit integers multiplication and
division indicate a 10-20% performance hit against `integer-gmp` but significant
speedup compared to `integer-simple`:

* [integer-openssl vs. integer-gmp](https://ch1bo.github.io/integer-openssl/openssl-vs-gmp.html)
* [integer-openssl vs. integer-simple](https://ch1bo.github.io/integer-openssl/openssl-vs-simple.html)

## License

The source code for `integer-openssl` is released under the [BSD-3-Clause License](https://opensource.org/licenses/BSD-3-Clause).

## TODO and ideas

- [ ] Implement the portable `GHC.Integer` interface
- [ ] Test suite covering all functions
  + [ ] `encodeDoubleInteger` fails for newer LTS / GHC?
  + [ ] test `gcdInteger`
  + [ ] test `lcmInteger`
- [ ] 32bit support
  + [ ] implement and test `int64ToInteger`
  + [ ] implement and test `word64ToInteger`
  + [ ] implement and test `integerToInt64`
  + [ ] implement and test `integerToWord64`
- [ ] Add some more short cuts
- [ ] Look into strictness/lazyness
- [ ] Common `Natural` implementation with integer-gmp (as its the same small/big abstraction)
- [ ] DRY code with integer-gmp
  + [ ] Integer functions in base and only BigNat/BigNum in libs?
  + [ ] Re-use BigNat/BigNum and exchange/use FFI/C parts if possible
- [ ] Rename library and flags to `integer-crypto`?
- [ ] Statically link `libcrypto` into GHC?


