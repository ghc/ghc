# Changelog for [`parallel` package](http://hackage.haskell.org/package/parallel)

## 3.2.2.0  *Jul 2018*

  - bump dependency bounds
  - add parEval
  - add a MonadFix instance

## 3.2.1.1  *Apr 2017*

  - Compatibility with `deepseq-1.4.3`
  - Minor documentation clarifications

## 3.2.1.0  *Jan 2016*

  - Support `base-4.9.0.0`
  - Add `{-# NOINLINE[1] rseq #-}` to make the `RULE` more robust
  - Fix broken links to papers in Haddock
  - Make `rpar` type signature consistent with `rseq` via type-synonym
  - Drop redundant `Ix`-constraint on `seqArray`/`seqArrayBounds` for GHC >= 8.0

## 3.2.0.6  *Dec 2014*

  - Make `-Wall` message free for all supported `base` versions

## 3.2.0.5  *Dec 2014*

  - Support `base-4.8.0.0`/`deepseq-1.4.0.0` (and thus GHC 7.10)

## 3.2.0.4  *Nov 2013*

  * Update package description to Cabal 1.10 format
  * Add support for GHC 7.8
  * Drop support for GHCs older than GHC 7.0.1
  * Add NOINLINE pragmas to `parBuffer`, `parList`, and `evalBuffer`
    to make RULEs more likely to fire

## Older versions

  * This package has a long history which is described in the Haddock documentation
    in the ["API History" section](./docs/Control-Parallel-Strategies.html#history)
