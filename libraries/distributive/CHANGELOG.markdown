0.6.2.1 [2020.12.30]
--------------------
* The build-type has been changed from `Custom` to `Simple`.
  To achieve this, the `doctests` test suite has been removed in favor of using
  [`cabal-docspec`](https://github.com/phadej/cabal-extras/tree/master/cabal-docspec)
  to run the doctests.

0.6.2 [2020.04.10]
------------------
* Make the `Distributive` instance for `Tagged` poly-kinded.

0.6.1 [2019.09.06]
------------------
* Add a `Distributive` instance for `WrappedMonad m`.

0.6 [2018.07.02]
----------------
* Remove `fmapCollect`. (See
  [here](https://github.com/ekmett/distributive/commit/1020655f15714514048d0dc842ffe4adcec89a7b)
  for an explanation of why it was removed.)
* Avoid incurring some dependencies when using recent GHCs.

0.5.3
-----
* Support `doctest-0.12`

0.5.2
-----
* Revamp `Setup.hs` to use `cabal-doctest`. This makes `distributive` build
  with `Cabal-1.25`, and makes the `doctest`s work with `cabal new-build` and
  sandboxes.
* Fix bugs in `Data.Distributive.Generic` that cause generic `Distributive`
  instances not to work properly for datatypes with recursive types
* Add `genericCollect` to `Data.Distributive.Generic`, and switch the underlying
  machinery in that module to work on a `collect`-like method instead of a
  `distribute`-like one
* Add a test suite for regression-testing `Data.Distributive.Generic`

0.5.1
-----
* Add `Distributive` instances for datatypes from `Data.Semigroup` and `GHC.Generics`
* Add `MINIMAL` pragma for `Distributive`

0.5.0.2
-------
* A more elegant fix for builds on GHC 7.2

0.5.0.1
-------
* Fix builds on GHC 7.2

0.5
---
* Added flags for removing some dependencies.
* Support `doctests` when building to non-standard locations (such as when using `stack`.)
* Support `base-orphans`

0.4.4
-----
* `transformers 0.4` compatibility

0.4.3.1
-----
* Fixed builds with older versions of GHC

0.4.2
-------
* Added `Data.Distributive.Generic`.

0.4.1
-----
* `Control.Monad.Instances` is deprecated in GHC 7.8. Don't import it there.

0.4
---
* Added support for `Data.Tagged` and `Data.Proxy`.

0.3.1
-----
* Minor documentation fix

0.3
---
* Added instances for `Control.Applicative.Backwards` and `Data.Functor.Reverse` from `transformers` 0.3, taking them from `transformers-compat` if necessary for `transformers` 0.2

