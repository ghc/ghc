0.8.6.1 [2020.12.28]
--------------------
* Mark all modules as explicitly Safe or Trustworthy.

0.8.6 [2018.07.02]
------------------
* Make the `Read(1)` instances for `Proxy` ignore the precedence argument,
  mirroring similar changes to `base`
  [here](http://git.haskell.org/ghc.git/commitdiff/8fd959998e900dffdb7f752fcd42df7aaedeae6e).
* Fix a bug in the `Floating` instance for `Tagged` in which `logBase` was
  defined in terms of `(**)`.
* Avoid incurring some dependencies when using recent GHCs.

0.8.5
-----
* Support `Data.Bifoldable`/`Data.Bitraversable` in `base` for GHC 8.1+.
* Backport the `Eq1`, `Ord1`, `Read1`, and `Show1` instances for `Proxy` from `base-4.9`
* Add `Eq1`/`2`, `Ord1`/`2`, `Read1`/`2`, and `Show1`/`2` instances for `Tagged`

0.8.4
-----
* Backport the `Alternative`, `MonadPlus`, and `MonadZip` instances for `Proxy` from `base-4.9`
* Add `Bits`, `FiniteBits`, `IsString`, and `Storable` instances for `Tagged`

0.8.3
-----
* Manual `Generic1` support to work around a bug in GHC 7.6
* Invert the dependency to supply the `Semigroup` instance ourselves when building on GHC 8

0.8.2
-------
* `deepseq` support.
* Widened `template-haskell` dependency bounds.

0.8.1
-----
* Add `KProxy` to the backwards compatibility `Data.Proxy` module.
* Add a `Generic` instance to `Proxy`.

0.8.0.1
-------
* Fix builds on GHC 7.4.

0.8
---
* Added `Data.Proxy.TH`, based on the code from `Frames` by Anthony Cowley.
* Removed `reproxy` from `Data.Proxy`. This is a bad API decision, but it isn't present in GHC's `Data.Proxy`, and this makes the API more stable.

0.7.3
---
* Support `Data.Bifunctor` in `base` for GHC 7.9+.

0.7.2
-----
* Fixed warning on GHC 7.8

0.7.1
-----
* Added `tagWith`.

0.7
---
* `Data.Proxy` has moved into base as of GHC 7.7 for use in the new `Data.Typeable`. We no longer export
  it for GHC >= 7.7. The most notable change in the module from the migration into base is the loss of
  the `reproxy` function.

0.6.2
-----
* Allowed polymorphic arguments where possible.

0.6.1
-----
* Needlessly claim that this entirely pure package is `Trustworthy`!

0.6
---
* On GHC 7.7, we now still export the instances we used to for `Data.Proxy.Proxy` as orphans if need be.

0.5
---
* On GHC 7.7 we now simply export `Data.Typeable.Proxy` rather than make our own type. We still re-export it.

0.4.5
-----
* Added `witness`

0.4.4
-----
* Actually working polymorphic kind support

0.4.3
-----
* Added polymorphic kind support
