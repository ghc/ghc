## 1.0.5 (2021-05-03)

- Add (Template Haskell) `Lift UUID` instance

## 1.0.4.0

- Declare `Data.UUID.Types` module "`Trustworthy`" with respect to SafeHaskell.
- Use more compact heap object representation which saves 16 bytes on 64bit platforms.
- Add `toWords64`/`fromWords64` functions
- Drop support for GHC < 7.
- Add support for `random-1.2`, i.e. idiomatic `Random` and added `Uniform` instances.

## 1.0.3

- Bump package dependencies.

## 1.0.2

- Add `toText`/`fromText` functions.

## 1.0.1

- Update dependencies in tests and benchmarks.

## 1.0.0

- Initial split from "`uuid-1.3.8`" package.
