# Revision history for `ghc-internal`

## 10.001.0 -- yyyy-mm-dd

* Add new `gc_sync_elapsed_ns` counter to GHC.Internal.Stats
* Fix a crash when decoding a captured stack containing a bytecode object with an empty payload bitmap ([GHC #26640](https://gitlab.haskell.org/ghc/ghc/-/issues/26640))

## 9.1401.0 -- yyyy-mm-dd

* Introduce `dataToCodeQ` and `liftDataTyped`, typed variants of `dataToExpQ` and `liftData` respectively.

## 9.1001.0 -- 2024-05-01

* Package created containing implementation moved from `base`.
