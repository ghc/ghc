## [0.2.14.0]

* [Add `HashMap.mapKeys`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/308) Thanks, Marco Perone!

* [Add instances for `NFData1` and `NFData2`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/314) Thanks, Isaac Elliott and Oleg Grenrus!

* [Fix `@since`-annotation for `compose`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/303) Thanks, @Mathnerd314!

[0.2.14.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.13.0...v0.2.14.0

## [0.2.13.0]

* [Add `HashMap.compose`.](https://github.com/haskell-unordered-containers/unordered-containers/pull/299) Thanks Alexandre Esteves.

[0.2.13.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.12.0...v0.2.13.0

## [0.2.12.0]

* Add `HashMap.isSubmapOf[By]` and `HashSet.isSubsetOf`. Thanks Sven Keidel. ([#282])

* Expose internal modules. ([#283])

* Documentation improvements in `Data.HashSet`, including a beginner-friendly
  introduction. Thanks Matt Renaud. ([#267])

* `HashMap.alterF`: Skip key deletion for absent keys. ([#288])

* Remove custom `unsafeShift{L,R}` definitions. ([#281])

* Various other documentation improvements.

[0.2.12.0]: https://github.com/haskell-unordered-containers/unordered-containers/compare/v0.2.11.0...v0.2.12.0
[#267]: https://github.com/haskell-unordered-containers/unordered-containers/pull/267
[#281]: https://github.com/haskell-unordered-containers/unordered-containers/pull/281
[#282]: https://github.com/haskell-unordered-containers/unordered-containers/pull/282
[#283]: https://github.com/haskell-unordered-containers/unordered-containers/pull/283
[#288]: https://github.com/haskell-unordered-containers/unordered-containers/pull/288

## 0.2.11.0

 * Add `HashMap.findWithDefault` (soft-deprecates `HashMap.lookupDefault`).
   Thanks, Matt Renaud.

 * Add `HashMap.fromListWithKey`. Thanks, Josef Svenningsson.

 * Add more folding functions and use them in `Foldable` instances. Thanks,
   David Feuer.

 * Add `HashMap.!?`, a flipped version of `lookup`. Thanks, Matt Renaud.

 * Add a `Bifoldable` instance for `HashMap`. Thanks, Joseph Sible.

 * Add a `HasCallStack` constraint to `(!)`. Thanks, Roman Cheplyaka.

### Bug fixes

 * Fix a space leak affecting updates on keys with hash collisions. Thanks,
   Neil Mitchell. ([#254])

 * Get rid of some silly thunks that could be left lying around. ([#232]).
   Thanks, David Feuer.

### Other changes

 * Speed up the `Hashable` instances for `HashMap` and `HashSet`. Thanks,
   Edward Amsden.

 * Remove a dependency cycle hack from the benchmark suite. Thanks,
   Andrew Martin.

 * Improve documentation. Thanks, Tristan McLeay, Li-yao Xia, Gareth Smith,
   Simon Jakobi, Sergey Vinokurov, and likely others.

[#232]: https://github.com/haskell-unordered-containers/unordered-containers/issues/232
[#254]: https://github.com/haskell-unordered-containers/unordered-containers/issues/254

## 0.2.10.0

 * Add `HashMap.alterF`.

 * Add `HashMap.keysSet`.

 * Make `HashMap.Strict.traverseWithKey` force the results before
   installing them in the map.

## 0.2.9.0

 * Add `Ord/Ord1/Ord2` instances. (Thanks, Oleg Grenrus)

 * Use `SmallArray#` instead of `Array#` for GHC versions 7.10 and above.
   (Thanks, Dmitry Ivanov)

 * Adjust for `Semigroup => Monoid` proposal implementation.
   (Thanks, Ryan Scott)

### Bug fixes

 * Fix a strictness bug in `fromListWith`.

 * Enable eager blackholing for pre-8.2 GHC versions to work around
   a runtime system bug. (Thanks, Ben Gamari)

 * Avoid sketchy reimplementation of `ST` when compiling with recent
   GHC.

### Other changes

 * Remove support for GHC versions before 7.8. (Thanks, Dmitry Ivanov)

 * Add internal documentaton. (Thanks, Johan Tibell)

## 0.2.8.0

 * Add `Eq1/2`, `Show1/2`, `Read1` instances with `base-4.9`

 * `Eq (HashSet a)` doesn't require `Hashable a` anymore, only `Eq a`.

 * Add `Hashable1/2` with `hashable-1.2.6.0`

 * Add `differenceWith` function.

## 0.2.7.2

 * Don't use -fregs-graphs

 * Fix benchmark compilation on stack.

## 0.2.7.1

 * Fix linker error related to popcnt.

 * Haddock improvements.

 * Fix benchmark compilation when downloaded from Hackage.

## 0.2.7.0

 * Support criterion 1.1

 * Add unionWithKey for hash maps.

## 0.2.6.0

 * Mark several modules as Trustworthy.

 * Add Hashable instances for HashMap and HashSet.

 * Add mapMaybe, mapMaybeWithKey, update, alter, and
   intersectionWithKey.

 * Add roles.

 * Add Hashable and Semigroup instances.

## 0.2.5.1 (2014-10-11)

 * Support base-4.8
