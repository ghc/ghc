5.0.8 [2020.12.30]
------------------
* Explicitly mark modules as Safe or Trustworthy.
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

5.0.7 [2020.12.15]
------------------
* Move `FunctorWithIndex (TracedT m w)` instance from `lens`.
  This instance depends on the `indexed-traversable` package. This can be disabled using the flag of the same name.

5.0.6 [2019.11.26]
------------------
* Achieve forward compatibility with
  [GHC proposal 229](https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0229-whitespace-bang-patterns.rst).

5.0.5 [2019.05.02]
------------------
* Raised the minimum `semigroups` version to 0.16.2. In addition, the
  package will only be required at all for GHCs before 8.0.
* Drop the `contravariant` flag from `comonad.cabal`, as `comonad` no longer
  depends on the `contravariant` library.

5.0.4 [2018.07.01]
------------------
* Add `Comonad` instances for `Tagged s` with `s` of any kind. Before the
  change, `s` had to be of kind `*`.
* Allow `containers-0.6`.

5.0.3 [2018.02.06]
------------------
* Don't enable `Safe` on GHC 7.2.

5.0.2
-----
* Support `doctest-0.12`

5.0.1
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes it build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.

5
-
* Removed module `Data.Functor.Coproduct` in favor of the `transformers`
  package's `Data.Functor.Sum`. n.b. Compatibility with older versions of
  `transformers` is possible using `transformers-compat`.
* Add `Comonad` instance for `Data.Functor.Sum.Sum`
* GHC 8 compatibility

4.2.7.2
-------
* Compiles warning-free on GHC 7.10

4.2.7.1
-------
* Use CPP

4.2.7
-----
* `Trustworthy` fixes for GHC 7.2

4.2.6
-----
* Re-export `(Data.Functor.$>)` rather than supply our own on GHC 7.8+
* Better SafeHaskell support.
* `instance Monoid m => ComonadTraced m ((->) m)`

4.2.5
-------
* Added a `MINIMAL` pragma to `Comonad`.
* Added `DefaultSignatures` support for `ComonadApply` on GHC 7.2+

4.2.4
-----
* Added Kenneth Foner's fixed point as `kfix`.

4.2.3
-----
* Add `Comonad` and `ComonadEnv` instances for `Arg e` from `semigroups 0.16.3` which can be used to extract the argmin or argmax.

4.2.2
-----
* `contravariant` 1.0 support

4.2.1
-----
* Added flags that supply unsupported build modes that can be convenient for sandbox users.

4.2
---
* `transformers 0.4` compatibility

4.1
---
* Fixed the 'Typeable' instance for 'Cokleisli on GHC 7.8.1

4.0.1
-----
* Fixes to avoid warnings on GHC 7.8.1

4.0
---
* Merged the contents of `comonad-transformers` and `comonads-fd` into this package.

3.1
---
* Added `instance Comonad (Tagged s)`.

3.0.3
-----
* Trustworthy or Safe depending on GHC version

3.0.2
-------
* GHC 7.7 HEAD compatibility
* Updated build system
