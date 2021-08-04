1.2.0.1 (2020-01-28)
====================

* Dependency bumps.

1.2.0.0 (2018-05-07)
====================

* Remove `IsString (m Doc)` instance (#16).

* Export the `beside` function from both modules.

    - For non-Monadic pretty-printing this is identical to the
      Semigroup/Monoid `<>` combinator.

    - For Monadic pretty-printing this is the lifted version of `<>`.

* No longer export `<>`.  Use `beside` instead, or the
  Semigroup/Monoid instance (for non-Monadic).

1.1.1.1 (2018-04-13)
====================

* Support for GHC-8.4.1.

1.1.1.0 (2017-01-26)
====================

* Support strict `Text` values (thanks to Elliot Cameron).

* Loosened constraints from `Monad` to `Functor` where applicable
  (thanks to Elliot Cameron).

* Uses `base-compat` to make it easier to support multiple GHC
  versions.

* Add `isEmpty :: Doc -> Bool`.

* Support `Semigroup` in `base >= 4.9.0.0`.

1.1.0.4 (2015-04-06)
====================

* Support `GHC-7.10`.

1.1.0.3 (2014-12-24)
====================

* Support `text-1.2.*`.

1.1.0.2 (2014-01-16)
====================

* Support `text-1.1.0.0`.

1.1.0.1 (2013-12-23)
====================

_This probably should have been 1.1.1.0._

* Documentation fixes.

* Add `displayB`, `spacebreak` and `<++>`.

* Smarter treatment of spaces, newlines, etc. (including how `line`
  behaves in `renderOneLine`).

1.1.0.0 (2012-08-22)
====================

* `Doc` is now an instance of `Monoid`.

* Add `renderOneLine` function.

* Add `IsString` instances.

* Make `SimpleDoc` an instance of `Show` for convenience.

* Make the `Show` instance for `Doc` match the documentation.

1.0.0.0 (2012-05-22)
====================

* Initial version,
