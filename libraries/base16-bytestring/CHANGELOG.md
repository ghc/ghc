# 1.0.1.0

* Backwards-compatible support for `bytestring ^>= 0.11` ([#15](https://github.com/haskell/base16-bytestring/pull/15)) 

# 1.0.0.0

* Merged omnibus PR doing a variety of things in ([#10](https://github.com/haskell/base16-bytestring/pull/10)):
  - Improves performance by 3-4x for encode, 4-5x for decode.
  - The `decode` signature returning the tuple and actually returns an error message with offset. The signature will now be `ByteString -> Either String ByteString`.
  - Actually tests using the test vectors defined in the RFC, and uses property tests to ensure invariants hold.
  - Adds lenient decoders to the API
  - Adds `-XTrustworthy` annotations to the relevant exposed modules
  - Rewrites the haddocks to be more up to date and fancy-styled.
  - Adds benchmarks to the `.cabal` file so they can be run at toplevel, and make them better.
  - Bumps the Cabal version to 1.12

Because of the breadth of this change, we are calling this a new epoch for the `base16-bytestring` library. Hence, the version `1.0.0.0`.

# 0.1.1.7

* Fix some bugs in lazy decoding
  ([#8](https://github.com/haskell/base16-bytestring/pull/8)).

# 0.1.1.6

*  Changelog not recorded up to this version.
