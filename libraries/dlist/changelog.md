# Change Log

## [Unreleased][]

_No unreleased changes at this time._

## [v1.0][]

Released on **2020-07-18**, **Nelson Mandela International Day**.

### Added

* `intercalate` for `DList` ([#43][], [Jacob Leach][])
* `Traversable` instance for `DList` ([#45][], [Veronika Romashkina][])
* `Data.DList.Internal` for the `DList` implementation, `Data.DList.Unsafe` for
  exporting the `DList` constructor `UnsafeDList` and record label
  `unsafeApplyDList` ([#55][], [#59][])
* `Data.DList.DNonEmpty` ([#60][])
* GitHub Action for uploading a release ([#74][])
* `dlist-bench`, a benchmark package ([#71][])

### Changed

* `stimes` for `DList` defined with `stimesMonoid` ([#46][], [Janek Spaderna][])
* Type of `tail`: `DList a -> DList a` to `DList a -> [a]` ([#69][])
* GitHub Action for continuous integration testing to replace Travis-CI
  ([#47][], [#50][])
* GHC warning and error improvements ([#72][], [#73][])
* Improved documentation ([#55][], [#70][], [#76][], [#77][])

### Removed

* `list :: b -> (a -> DList a -> b) -> DList a -> b` ([#69][])

## [v0.8.0.8][]

Released on **2020-04-02**, **World Autism Awareness Day**.

### Added

* `toList` in the `Foldable` instance for `DList` ([#36][], [Ryan Scott][])

### Changed

* `QuickCheck` upper bound: 2.14 to 2.15 ([`a7ea60d`][])

### Fixed

* Documented time complexity of `head` for `DList` ([#35][], [Simon Jakobi][])

## [v0.8.0.7][]

Released on **2019-08-05**, **Independence Day in Burkina Faso**.

### Added

* `MonadFail` instance for `DList` ([#32][], [Vanessa McHale][])

### Changed

* `deepseq` upper bound: 2 to 1.5 ([#33][], [Herbert Valerio Riedel][])

## [v0.8.0.6][]

Released on **2019-03-29**, **Martyrs' Day in Madagascar**.

### Changed

* `QuickCheck` upper bound: 2.13 to 2.14 ([`242511c`][])

## [v0.8.0.5][]

Released on **2018-09-13**, **Day of the Programmer**.

### Changed

* `QuickCheck` upper bound: 2.12 to 2.13 ([`0e2b3a5`][])

## [v0.8.0.4][]

Released on **2018-01-19**, **Kokborok Day**.

### Added

* `{-# LANGUAGE Trustworthy #-}` in `Data.DList` ([#31][], [Bertram
  Felgenhauer][])

### Changed

* `QuickCheck` upper bound: 2.11 to 2.12 ([`3d9c8ad`][])
* `QuickCheck` lower bound: 2.7/2.9 to 2.10 ([`4f92012`][])
* `Arbitrary`, `Arbitrary1` instances for `NonEmpty` in the test suite copied
  from `quickcheck-instances` ([`4f92012`][])

## [v0.8.0.3][]

Released on **2017-07-04**, **Independence Day in the United States**.

### Added

* `quickcheck-instances` dependency in the test suite for the `Arbitrary`,
  `Arbitrary1` instances for `NonEmpty` ([`5b41d0d`][])

### Changed

* `QuickCheck` upper bound: 2.10 to 2.11 ([`b2f791a`][])

### Fixed

* `stimes` property in the test suite ([#30][], [Oleg Grenrus][])

## [v0.8.0.2][]

Released on **2016-09-04**, **World Sexual Health Day**.

### Fixed

* Missing module `OverloadedStrings` in the test suite ([#29][], [Sergei
  Trofimovich][])

## [v0.8.0.1][]

Released on **2016-07-29**, the **58th Anniversary of the Creation of NASA**.

### Changed

* `QuickCheck` lower bound: 2.7 to 2.9 for GHC >= 8 ([#28][], [Adam Bergmark][])

## [v0.8][]

Released on **2016-07-17**, **Constitution Day in South Korea**.

### Added

* Pattern synonyms `Nil` and `Cons` ([#15][])
* `Semigroup` instance for `DList` ([#25][])
* Canonical `Applicative` and `Monad` instances for `DList` ([#23][], [Herbert
  Valerio Riedel][])

### Changed

* `IsString` instance for `DList` is no longer flexible ([#26][], [Baldur
  Blöndal][])
* `QuickCheck` upper bound: 2.9 to 2.10 ([`ef7eac5`][])

## [v0.7.1.2][]

Released on **2015-08-23**, **International Day for the Remembrance of the Slave
Trade and its Abolition**.

### Fixed

* Imports causing warnings in GHC 7.10 ([#22][], [Mikhail Glushenkov][])

## [v0.7.1.1][]

Released on **2015-03-19**, **St. Joseph's Day**.

### Changed

* `QuickCheck` lower bound: 2.5 to 2.7 ([`2d8ec37`][])
* `QuickCheck` upper bound: 2.8 to 2.9 ([`3176153`][])

## [v0.7.1][]

Released on **2014-06-28**, the **100th Anniversary of the Assassination of
Franz Ferdinand**.

### Added

* `IsList` instance for `DList` ([#13][], [Baldur Blöndal][])

## [v0.7.0.1][]

Released on **2014-03-24**, **World Tuberculosis Day**.

### Changed

* `QuickCheck` upper bound: 2.7 to 2.8 ([`7494dbc`][])

## [v0.7][]

Released on **2014-03-17**, **St. Patrick's Day**.

### Added

* `NFData` instance for `DList` ([#10][], [Evan Laforge][])
* `IsString` instance for `DList` ([`771a38d`][])

### Changed

* `base` lower bound: 2 to 4 ([`77f6898`][])

### Removed

* `DList` constructor and record label, `maybeReturn` ([`62c0c09`][])

## [v0.6.0.1][]

Released on **2013-12-01**, **World AIDS Day**.

### Changed

* `QuickCheck` lower bound: 2.6 to 2.5 ([#9][], [Michael Snoyman][])

## [v0.6][]

Released on **2013-11-29**, **Black Friday**.

### Added

* `apply` to replace `DList` record label `unDL` ([#4][])
* `Eq`, `Ord`, `Show`, and `Alternative` instances for `DList` ([#1][], [Bas van
  Dijk][])
* `Read` instance for `DList` ([`58ef305`][])
* `Foldable` instance for `DList` ([`5b1d09f`][])
* Travis-CI for continuous integration testing ([#6][], [Herbert Valerio
  Riedel][])

### Changed

* Maintenance: [Don Stewart][] to [Sean Leather][] ([#2][], [Bas van Dijk][])
* Repository: `http://code.haskell.org/~dons/code/dlist/` to
  `https://github.com/spl/dlist`
* `base` lower bound: 0 to 2 ([`6e1d9e7`][])

### Fixed

* Test suite simplified and changed to use `cabal test` ([`9f58759`][])

### Deprecated

* Exported `DList` constructor and record label, `maybeReturn` ([#4][])

<!-- Keep each of these groups of references sorted. -->

<!-- Version tag comparisons -->

[v0.6]: https://github.com/spl/dlist/compare/v0.5...v0.6
[v0.6.0.1]: https://github.com/spl/dlist/compare/v0.6...v0.6.0.1
[v0.7]: https://github.com/spl/dlist/compare/v0.6.0.1...v0.7
[v0.7.0.1]: https://github.com/spl/dlist/compare/v0.7...v0.7.0.1
[v0.7.1.1]: https://github.com/spl/dlist/compare/v0.7.1...v0.7.1.1
[v0.7.1.2]: https://github.com/spl/dlist/compare/v0.7.1.1...v0.7.1.2
[v0.7.1]: https://github.com/spl/dlist/compare/v0.7.0.1...v0.7.1
[v0.8]: https://github.com/spl/dlist/compare/v0.7.1.2...v0.8
[v0.8.0.1]: https://github.com/spl/dlist/compare/v0.8...v0.8.0.1
[v0.8.0.2]: https://github.com/spl/dlist/compare/v0.8.0.1...v0.8.0.2
[v0.8.0.3]: https://github.com/spl/dlist/compare/v0.8.0.2...v0.8.0.3
[v0.8.0.4]: https://github.com/spl/dlist/compare/v0.8.0.3...v0.8.0.4
[v0.8.0.5]: https://github.com/spl/dlist/compare/v0.8.0.4...v0.8.0.5
[v0.8.0.6]: https://github.com/spl/dlist/compare/v0.8.0.5...v0.8.0.6
[v0.8.0.7]: https://github.com/spl/dlist/compare/v0.8.0.6...v0.8.0.7
[v0.8.0.8]: https://github.com/spl/dlist/compare/v0.8.0.7...v0.8.0.8
[v1.0]: https://github.com/spl/dlist/compare/v0.8.0.8...v1.0

<!-- The HEAD version tag comparison -->

[Unreleased]: https://github.com/spl/dlist/compare/v1.0...HEAD

<!-- Issues and PRs -->

[#1]: https://github.com/spl/dlist/pull/1
[#2]: https://github.com/spl/dlist/pull/2
[#4]: https://github.com/spl/dlist/issues/4
[#6]: https://github.com/spl/dlist/pull/6
[#9]: https://github.com/spl/dlist/pull/9
[#10]: https://github.com/spl/dlist/issues/10
[#13]: https://github.com/spl/dlist/pull/13
[#15]: https://github.com/spl/dlist/issues/15
[#22]: https://github.com/spl/dlist/pull/22
[#23]: https://github.com/spl/dlist/pull/23
[#25]: https://github.com/spl/dlist/issues/25
[#26]: https://github.com/spl/dlist/pull/26
[#28]: https://github.com/spl/dlist/issues/28
[#29]: https://github.com/spl/dlist/pull/29
[#30]: https://github.com/spl/dlist/pull/30
[#31]: https://github.com/spl/dlist/pull/31
[#32]: https://github.com/spl/dlist/pull/32
[#33]: https://github.com/spl/dlist/pull/33
[#35]: https://github.com/spl/dlist/pull/35
[#36]: https://github.com/spl/dlist/pull/36
[#43]: https://github.com/spl/dlist/pull/43
[#45]: https://github.com/spl/dlist/pull/45
[#46]: https://github.com/spl/dlist/pull/46
[#47]: https://github.com/spl/dlist/pull/47
[#50]: https://github.com/spl/dlist/pull/50
[#55]: https://github.com/spl/dlist/pull/55
[#59]: https://github.com/spl/dlist/pull/59
[#60]: https://github.com/spl/dlist/pull/60
[#69]: https://github.com/spl/dlist/pull/69
[#70]: https://github.com/spl/dlist/pull/70
[#71]: https://github.com/spl/dlist/pull/71
[#72]: https://github.com/spl/dlist/pull/72
[#73]: https://github.com/spl/dlist/pull/73
[#74]: https://github.com/spl/dlist/pull/74
[#76]: https://github.com/spl/dlist/pull/76
[#77]: https://github.com/spl/dlist/pull/77

<!-- Commits -->

[`0e2b3a5`]: https://github.com/spl/dlist/commit/0e2b3a542796b50796f2aa6dde4665911b9d15a1
[`242511c`]: https://github.com/spl/dlist/commit/242511c501299b38c57efeafb9e604f29cb8bb7a
[`2d8ec37`]: https://github.com/spl/dlist/commit/2d8ec370a3c19d39c0d543f39f8fc31948087fd9
[`3176153`]: https://github.com/spl/dlist/commit/3176153187b130002a1577675cdcd5509dd86556
[`3d9c8ad`]: https://github.com/spl/dlist/commit/3d9c8ad348b419590a121b8a1604e8ebd01bffbe
[`4f92012`]: https://github.com/spl/dlist/commit/4f920128592f6f99b8c57a1adf50cdb16d26c13b
[`58ef305`]: https://github.com/spl/dlist/commit/58ef305146474d77a49a3f9e0148393eb6546fd2
[`5b1d09f`]: https://github.com/spl/dlist/commit/5b1d09f6daad5543d927a003b4ea5ca50f3e6604
[`5b41d0d`]: https://github.com/spl/dlist/commit/5b41d0d84a0a14c75798ca30883b613b37ad464a
[`62c0c09`]: https://github.com/spl/dlist/commit/62c0c099d20c3f950d7950dc9ec5a6b3797acaf8
[`6e1d9e7`]: https://github.com/spl/dlist/commit/6e1d9e74e0a7c7f9c6612cd6bd0b4753f5651968
[`7494dbc`]: https://github.com/spl/dlist/commit/7494dbc56550a0f7eb09304403a61c68b4a360e3
[`771a38d`]: https://github.com/spl/dlist/commit/771a38df953b97a631806884133a76ab8dfcfce8
[`77f6898`]: https://github.com/spl/dlist/commit/77f689829223b5fd6762e24594ce9111e6ef8f6b
[`9f58759`]: https://github.com/spl/dlist/commit/9f587599f128a4dc147c5c8f907b29b46110763b
[`a7ea60d`]: https://github.com/spl/dlist/commit/a7ea60d3d02775216a15d6f688db230d7735c9d1
[`b2f791a`]: https://github.com/spl/dlist/commit/b2f791ab98e2091303fff4567727716b6021b63e
[`ef7eac5`]: https://github.com/spl/dlist/commit/ef7eac55fc7e180ac3441657f4971ed171b0669c

<!-- Contributors -->

[Adam Bergmark]: https://github.com/bergmark
[Baldur Blöndal]: https://github.com/Icelandjack
[Bas van Dijk]: https://github.com/basvandijk
[Bertram Felgenhauer]: https://github.com/int-e
[Don Stewart]: https://github.com/donsbot
[Evan Laforge]: https://github.com/elaforge
[Herbert Valerio Riedel]: https://github.com/hvr
[Jacob Leach]: https://github.com/riz0id
[Janek Spaderna]: https://github.com/JaSpa
[Michael Snoyman]: https://github.com/snoyberg
[Mikhail Glushenkov]: https://github.com/23Skidoo
[Oleg Grenrus]: https://github.com/phadej
[Ryan Scott]: https://github.com/RyanGlScott
[Sean Leather]: https://github.com/spl
[Sergei Trofimovich]: https://github.com/trofi
[Simon Jakobi]: https://github.com/sjakobi
[Vanessa McHale]: https://github.com/vmchale
[Veronika Romashkina]: https://github.com/vrom911
